{-# LANGUAGE BlockArguments #-}
module Controller where
import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Control.Arrow (Arrow(second))
import Foreign (toBool)
import GHC.Base (VecElem(Int16ElemRep))
import Data.Data (ConstrRep(FloatConstr))

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | state gstate == Playing = regulateState $ ghostStateCheck $ collision $ movement $ spawnCherry gstate { elapsedTime = elapsedTime gstate + secs }
                 | otherwise = regulateState gstate { elapsedTime = elapsedTime gstate + secs }  --If gameplay is paused or Ended no need to do movement or other things

-- | Update the movement for the game state
movement :: GameState -> GameState
movement gstate = gstate { pacman = pacMovement (pacman gstate) (grid gstate), 
                           ghosts = ghostMovement (ghosts gstate) (grid gstate) (pacman gstate) }

pacMovement :: Pacman -> Grid -> Pacman  -- | Pacman movement for each step
pacMovement pacman_@(Pac {pacPos = pacPos, pacDir = currDir, pacDesDir = desDir}) grid
  | validDirection pacPos desDir grid  = pacman_ { pacPos = goToDirection pacPos speed desDir, pacDir = desDir}
  | validDirection pacPos currDir grid = pacman_ { pacPos = goToDirection pacPos speed currDir}
  | otherwise                          = pacman_ { pacDir = X }
    where 
      speed = 2

-- | Increment the given point with a given speed in the given direction 
goToDirection :: Point -> Float -> Direction -> Point
goToDirection (x, y) speed dir | dir == N = (x        , y + speed)
                         | dir == E = (x + speed, y        )
                         | dir == S = (x        , y - speed)
                         | dir == W = (x - speed, y        )
                         | dir == X = (x        , y        )

-- | Check for a given point and direction in a grid if it is a valid direction to go
validDirection :: Point -> Direction -> Grid -> Bool
validDirection pos dir grid = not firstWallCheck && not secondWallCheck
  where 
    wallCheckPointsTuple = wallCheckPoints pos dir
    firstWallCheck = wallCheck  (gamePosToGridPosExact $ fst wallCheckPointsTuple) grid 
    secondWallCheck = wallCheck (gamePosToGridPosExact $ snd wallCheckPointsTuple) grid 

gamePosToGridPosExact :: Point -> Point
gamePosToGridPosExact (0, 0) = (0                  , 0)  
gamePosToGridPosExact (x, 0) = (floorFloat (x / 20), 0)
gamePosToGridPosExact (0, y) = (0                  , floorFloat (-y / 20))
gamePosToGridPosExact (x, y) = (floorFloat (x / 20), floorFloat (-y / 20))

-- | Round the Float to its nearest Int and make it a Float again
floorFloat :: Float -> Float
floorFloat x = fromIntegral $ floor x

-- | Get the points for a sprite location that need to be checked if there is a wall
wallCheckPoints :: Point -> Direction -> (Point, Point)
wallCheckPoints (x, y) dir | dir == N = ((x+extra, y + 1), (x + 20 - extra, y + 1))
                           | dir == E = ((x + 21, y - extra), (x + 21, y - 20 + extra))
                           | dir == S = ((x+ extra, y - 21), (x + 20- extra, y - 21))
                           | dir == W = ((x - 1, y- extra), (x - 1, y -20+ extra))
                           | dir == X = ((x, y), (x, y))
                              where
                                extra = 1

-- | Given a location get the middle of that sprite
centerPointOfSprite :: Point -> Point
centerPointOfSprite point@(x,y) = (x + 10.5, y + 10.5)

-- | Given a position on a grid state if there is a wall
wallCheck :: Point -> Grid -> Bool
wallCheck pos grid@(square@(fieldPos, field): restGrid) 
  | pos == fieldPos = field == Wall
  | otherwise       = wallCheck pos restGrid

  -- | GHOST movement for each step
ghostMovement :: [Ghost] -> Grid -> Pacman -> [Ghost]
ghostMovement ghosts grid pacman_@(Pac{pacPos = pacPos_, pacDir = pacDir_}) = map singularGhostMovement ghosts
  where 
    singularGhostMovement :: Ghost -> Ghost
    singularGhostMovement ghost_@(Gho {ghostPos = ghoPos_, ghostDir = ghodir})
      -- | There is more than one valid direction meaning there is a crossroad thus we use the ghostDirectionDecider
      | length validDirections > 1 = ghost_ {ghostPos = goToDirection ghoPos_ speed ghostDirectionDecider, ghostDir = ghostDirectionDecider}
      -- | There is one valid direction thus we go there (it is a corner or straight path)
      | length validDirections == 1 =  ghost_ {ghostPos = goToDirection ghoPos_ speed (head validDirections), ghostDir = head validDirections} 
      -- | There are no other paths so we stop
      | otherwise =  ghost_ {ghostPos = ghoPos_, ghostDir = X} 
        where
          speed | ghostState ghost_ == Run  = 1
                | ghostState ghost_ == Dead = 1
                | ghostType ghost_ == Blinky = 2
                | otherwise                 = 2
          validDirections = map snd (filter fst (zip directsBool directs))
          directs = [N, E, S, W]
          directsBool = [north, east, south, west]
          north = ghodir /= S && validDirection ghoPos_ N grid 
          east  = ghodir /= W && validDirection ghoPos_ E grid 
          south = ghodir /= N && validDirection ghoPos_ S grid
          west  = ghodir /= E && validDirection ghoPos_ W grid 

          ghostDirectionDecider :: Direction
          ghostDirectionDecider   | gstate == Run   = runAlgoritm -- Go a random dir when on a crossroad
                                  | gstate == Dead  = deadAlgoritm -- Go back to spawn
                                  | gtype == Blinky = blinkyAlgoritm -- Go to pacman
                                  | gtype == Pinky  = pinkyAlgoritm -- Go to two spaces infront of pacman
                                  | gtype == Clyde  = clydeAlgoritm -- Go to pacman but turn around when in an 4 dot radius
                                  | otherwise       = inkyAlgoritm -- Go to the avarage of pacman and blinky
            where
              gstate = ghostState ghost_
              gtype = ghostType ghost_

              -- Compute the score to for each validDirection to a given point
              directionsWithScore :: Point -> [(Float, Direction)]
              directionsWithScore desPos = zip (map (directionValue desPos) validDirections) validDirections

              blinkyAlgoritm = snd $ bestDir (directionsWithScore pacPos_)
              pinkyAlgoritm = snd $ bestDir (directionsWithScore (goToDirection pacPos_ 40 pacDir_))
              clydeAlgoritm = snd $ clydeDir (directionsWithScore pacPos_)
              inkyAlgoritm = snd $ bestDir (directionsWithScore inkyDesPos)
              deadAlgoritm = snd $ bestDir (directionsWithScore middleOfGrid) -- Temporary for the place where they go when die
              runAlgoritm = snd $ worstDir (directionsWithScore pacPos_) -- Temporary for the place where they go when run

              middleOfGrid = (x, y)
                where 
                  (x, y) = (200,-200)

              inkyDesPos = ((x1 + x2) / 2, (y1 + y2) / 2) 
                where 
                  (x1, y1) = pacPos_
                  (x2, y2) = ghostPos (getBlinky ghosts)

                  getBlinky :: [Ghost] -> Ghost
                  getBlinky (currGhost : ghostsB) | ghostType currGhost == Blinky = currGhost
                                                  | otherwise = getBlinky ghostsB

              clydeDir :: [(Float, Direction)] -> (Float, Direction)
              clydeDir [] = error "don't give an empty list to this function"
              clydeDir (x : xs)   = clydeDirHelper x xs

              clydeDirHelper :: (Float, Direction) -> [(Float, Direction)] -> (Float, Direction)
              clydeDirHelper currDir [] = currDir
              clydeDirHelper currDir (x : xs) | fst x < fst currDir && fst x > 80 = clydeDirHelper x xs 
                                              | otherwise                        = clydeDirHelper currDir xs
              worstDir :: [(Float, Direction)] -> (Float, Direction)
              worstDir [] = error "don't give an empty list to this function"
              worstDir (x : xs)   = worstDirHelper x xs

              worstDirHelper :: (Float, Direction) -> [(Float, Direction)] -> (Float, Direction)
              worstDirHelper currDir [] = currDir
              worstDirHelper currDir (x : xs) | fst x > fst currDir = worstDirHelper x xs 
                                              | otherwise           = worstDirHelper currDir xs

              bestDir :: [(Float, Direction)] -> (Float, Direction)
              bestDir [] = error "don't give an empty list to this function"
              bestDir (x : xs)   = bestDirHelper x xs

              bestDirHelper :: (Float, Direction) -> [(Float, Direction)] -> (Float, Direction)
              bestDirHelper currDir [] = currDir
              bestDirHelper currDir (x : xs) | fst x < fst currDir = bestDirHelper x xs 
                                             | otherwise           = bestDirHelper currDir xs

              directionValue :: Point -> Direction -> Float
              directionValue desPos dir | dir == N  = distBetweenPacman (xg, yg + speed)
                                        | dir == E  = distBetweenPacman (xg + speed, yg)
                                        | dir == S  = distBetweenPacman (xg, yg - speed)
                                        | otherwise = distBetweenPacman (xg - speed, yg) 
                                          where
                                            (xg, yg) = ghoPos_
                                            (x1, y1) = desPos 
                                            distBetweenPacman (x2, y2) = sqrt( (x2 - x1) ^ 2 + (y2 - y1) ^ 2)
                            
ghostStateCheck :: GameState -> GameState
ghostStateCheck gstate = gstate {ghosts = map ghostStateCheck' (ghosts gstate)}
  where
    ghostStateCheck' :: Ghost -> Ghost
    ghostStateCheck' ghost | ghostState ghost == Dead && validPos (ghostPos ghost) = ghost{ghostState = Normal}
                           | ghostState ghost == Run && elapsedTime gstate >5 = ghost{ghostState = Normal}
                           | otherwise = ghost
                            where 
                              validPos :: Point -> Bool
                              validPos (x, y) = 160 <= x && x <= 240 && (-180) >= y && y >= (-220)


-- | Update the collision for the game state
collision :: GameState -> GameState
collision gstate = ghostCollisions $ fieldCollision gstate

-- | Given a gamestate handle the collision for pacman with fields
fieldCollision :: GameState -> GameState
fieldCollision gstate_
    | power = gstate_ { grid = newGrid, ghosts = updateGhostForPowerPellet power (ghosts gstate_), score = (score gstate_) {currScore = newScore}, elapsedTime = 0}
    | otherwise = gstate_ { grid = newGrid, score = (score gstate_) {currScore = newScore}}
    where 
      (newGrid, newScore, power) = fieldCollisionHelper (gamePosToGridPos (pacPos (pacman gstate_))) (grid gstate_) (currScore (score gstate_))

-- | Given pacman's position a grid and the current score, update the grid, score and a bool stating if pacman ate a power pellet
fieldCollisionHelper :: Point -> Grid -> Int -> (Grid, Int, Bool)
fieldCollisionHelper pos grid@(square@(fieldPos, field): restGrid) oldScore 
                    | pos == fieldPos = (newSquare : restGrid, newScore, power)
                    | otherwise       = (square : recursionGrid, recursionScore, newBool)
                      where
                        (newSquare, newScore, power) = squarePacmanCollision square oldScore
                        (recursionGrid, recursionScore, newBool) = fieldCollisionHelper pos restGrid oldScore

-- | Given a square pacman collides with return what the square should turn into, the new score and if it was a Power Pellet
squarePacmanCollision :: Square -> Int -> (Square, Int, Bool)
squarePacmanCollision square@(fieldPos, field) score | field == Pellet = ((fieldPos, Empty), score + 10, False)
                                                     | field == Power  = ((fieldPos, Empty), score + 100, True)
                                                     | field == Cherry = ((fieldPos, Empty), score + 500, False)
                                                     | otherwise = (square, score, False)

-- | If given True make all the ghosts get affected by the PowerPellet
updateGhostForPowerPellet :: Bool -> [Ghost] -> [Ghost]
updateGhostForPowerPellet False ghosts = ghosts 
updateGhostForPowerPellet True ghosts = map makeGhostRun ghosts

-- | Give a ghost and make it run if it is in its normal state
makeGhostRun :: Ghost -> Ghost
makeGhostRun ghost | ghostState ghost == Normal = ghost{ ghostState = Run} 
                   | otherwise                  = ghost

-- | Return the closest grid position for a given game position for sprites
gamePosToGridPos :: Point -> Point
gamePosToGridPos (0, 0) = (0                  , 0)  
gamePosToGridPos (x, 0) = (roundFloat (x / 20), 0)
gamePosToGridPos (0, y) = (0                  , roundFloat (-y / 20))
gamePosToGridPos (x, y) = (roundFloat (x / 20), roundFloat (-y / 20))

-- | Round the Float to its nearest Int and make it a Float again
roundFloat :: Float -> Float
roundFloat x = fromIntegral $ round x

-- | Handle the collisions with ghosts
ghostCollisions :: GameState -> GameState
ghostCollisions gstate_ 
    | pacmanHurt = resetLvl gstate_
    | otherwise  = gstate_ {ghosts = newGhosts}
    where 
      newGhosts = map fst ghostBools
      ghostBools = map (handleGhostCollison . checkGhostCollision (pacPos (pacman gstate_))) (ghosts gstate_)
      pacmanHurt = any snd ghostBools

-- | Given a ghost and if it collided with pacman handle the collision
handleGhostCollison :: (Ghost, Bool) -> (Ghost, Bool)
handleGhostCollison (ghost@(Gho{ghostState = ghostState_}), collided) 
    | not collided = (ghost, collided)
    | ghostState_ == Run = (ghost{ ghostState = Dead}, False)
    | ghostState_ == Normal = (ghost, True)
    | ghostState_ == Dead = (ghost, False)

-- | Given pacman's position and a ghost determine if they collide
checkGhostCollision :: Point -> Ghost -> (Ghost, Bool)
checkGhostCollision (xP, yP) ghost@(Gho{ghostPos = (xG,yG)}) 
  | (xP > xG && xP - dangerDistance > xG) || (xP < xG && xP + dangerDistance < xG)
    || (yP > yG && yP - dangerDistance > yG) || (yP < yG && yP + dangerDistance < yG) = (ghost, False)
  | otherwise = (ghost, True)
    where
      -- If we are strict that when bounding boxes collide pacman dies it feels unfair thus we introduced pitty giving pacman some room
      dangerDistance = pacmanWidth - pitty
      pacmanWidth = 20
      pitty = 4

-- | Regulate the state for the game state
regulateState :: GameState -> IO GameState
regulateState gstate | state gstate == Starting && elapsedTime gstate >= timeToStart = return gstate { state = Playing }
                     | state gstate == Ended && (currScore (score gstate) > highScore (score gstate)) = do 
                                                                                                        writeFile "./Txtfiles/HighScore.txt" (show (currScore (score gstate))) 
                                                                                                        return gstate { score = (score gstate) {highScore = currScore (score gstate)}} --should prevent infinitely writing this file again and again.
                     | state gstate == Playing && not (anyPallets (grid gstate)) = followUpState randomNumber gstate {rng = newGen}
                     | otherwise = return gstate
  where timeToStart = 3
        (randomNumber, newGen) = randomR (1,3) (rng gstate)  :: (Int, StdGen)
anyPallets :: Grid -> Bool
anyPallets = any isPallet
  where isPallet ((_, _), field) = field == Pellet

-- | Spawning randomly a cherry at a random spot.
spawnCherry :: GameState -> GameState
spawnCherry gstate | randomNumber == 1 = gstate {grid = cherryInserter newGen $ grid gstate, rng = newGen }
                   | otherwise = gstate{rng = newGen}
  where generator = rng gstate
        (randomNumber, newGen) = randomR (1, cherrySpawnChance) generator  :: (Int, StdGen)
        cherrySpawnChance = 600 --1/chance so the higher this number the lower the chance

cherryInserter :: StdGen -> Grid -> Grid
cherryInserter gen grid_ | empties /= [] = replaceAt' ( findSq randomNumber Cherry empties ) grid_
                         | otherwise = grid_
  where
    (randomNumber, newGen) = randomR (0, length empties - 1) gen  :: (Int, StdGen)
    empties = filter isEmpty grid_
    isEmpty ((_, _), field) = field == Empty

findSq :: Int -> Field -> [Square] -> Square --No need for base case as it has been made sure that the int is within the range of spots.
findSq 0 f ((x,_):xs) = (x,f)
findSq i f (x:xs) = findSq (i - 1) f xs

replaceAt' :: Square -> [Square] -> [Square]
replaceAt' _ [] = []
replaceAt' sq@(x,f) (ff@(x',_):xs) | x == x' = sq : xs
                                   | otherwise = ff : replaceAt' sq xs


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate | pausedOrEnded == Paused = return (inputUnPause e gstate)
               | pausedOrEnded == Ended = do 
                                          
                                            inputNewGame e gstate
               | pausedOrEnded == Starting = return gstate
               | otherwise = return (inputKey e gstate) --Playing && Starting
  where pausedOrEnded = state gstate

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey c) Down _ _) gstate@GameState{ pacman = pacman_ }
  | c == KeySpace = gstate { state = Paused }
  | c == KeyShiftL = gstate { state = Ended }
  | c == KeyLeft  = gstate { pacman = pacman_ {pacDesDir = W}}
  | c == KeyRight = gstate { pacman = pacman_ {pacDesDir = E}}
  | c == KeyUp    = gstate { pacman = pacman_ {pacDesDir = N}}
  | c == KeyDown  = gstate { pacman = pacman_ {pacDesDir = S}}
inputKey _ gstate = gstate -- Otherwise keep the same.

inputUnPause :: Event -> GameState -> GameState
inputUnPause (EventKey (SpecialKey c) Down _ _) gstate
  | c == KeySpace = gstate { state = Playing }   -- Unpause the game
inputUnPause _ gstate = gstate -- Otherwise keep the same.

inputNewGame :: Event -> GameState -> IO GameState
inputNewGame (EventKey (SpecialKey c) Down _ _) gstate
  | c == KeyEnter && state gstate == Ended = initialState  -- Restart the game !!Random or gstate Int to keep track
inputNewGame _ gstate = return gstate -- Otherwise keep the same.
