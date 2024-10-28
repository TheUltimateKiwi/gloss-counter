{-# LANGUAGE BlockArguments #-}
module Controller where
import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Control.Arrow (Arrow(second))
import Foreign (toBool)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | state gstate == Playing = return $ collision $ movement $ spawnCherry gstate
                 | otherwise = regulateState gstate { elapsedTime = elapsedTime gstate + secs }  --If gameplay is paused or Ended no need to do movement or other things

-- | Update the movement for the game state
movement :: GameState -> GameState
movement gstate = gstate { pacman = pacMovement (pacman gstate) (grid gstate)}--, ghosts = ghostMovement $ ghosts gstate}

pacMovement :: Pacman -> Grid -> Pacman  -- | Pacman movement for each step
pacMovement pacman_@(Pac {pacPos = pacPos, pacDir = currDir, pacDesDir = desDir}) grid
  | validDirection pacPos desDir grid  = pacman_ { pacPos = goToDirection pacPos speed desDir, pacDir = desDir}
  | validDirection pacPos currDir grid = pacman_ { pacPos = goToDirection pacPos speed currDir}
  | otherwise                     = pacman_ { pacDir = X }
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
validDirection pos dir grid = firstWallCheck || secondWallCheck
  where 
    wallCheckPointsTuple = wallCheckPoints pos dir
    firstWallCheck = wallCheck  (gamePosToGridPos $ fst wallCheckPointsTuple) grid 
    secondWallCheck = wallCheck (gamePosToGridPos $ snd wallCheckPointsTuple) grid 

-- | Get the points for a sprite location that need to be checked if there is a wall
wallCheckPoints :: Point -> Direction -> (Point, Point)
wallCheckPoints (x, y) dir | dir == N = ((x, y + 1), (x + 20, y + 1))
                        | dir == E = ((x + 21, y), (x + 21, y - 20))
                        | dir == S = ((x, y - 21), (x + 20, y - 21))
                        | dir == W = ((x - 1, y), (x - 1, y -20))
                        | dir == X = ((x, y), (x, y))

-- | Given a location get the middle of that sprite
centerPointOfSprite :: Point -> Point
centerPointOfSprite point@(x,y) = (x + 10, y + 10)

-- | Given a position on a grid state if there is a wall
wallCheck :: Point -> Grid -> Bool
wallCheck pos grid@(square@(fieldPos, field): restGrid) 
  | pos == fieldPos = field == Wall
  | otherwise       = wallCheck pos restGrid

  -- | GHOST movement for each step
ghostMovement :: [Ghost] -> [Ghost]
ghostMovement = map singularGhostMovement

singularGhostMovement :: Ghost -> Ghost
singularGhostMovement ghost_@(Gho {ghostPos = ghopos, ghostDir = ghodir})
  | isAtCrossRoad ghopos = let decidedDir = ghostDirectionDecider ghost_
                            in ghost_ {ghostPos = goToDirection ghopos speed decidedDir, ghostDir = decidedDir}
  | otherwise                = ghost_ {ghostPos = goToDirection ghopos speed ghodir}
    where
      speed = 1

isAtCrossRoad :: Point -> Bool
isAtCrossRoad point = undefined -- implement checking if the ghost is at a crossroad

ghostDirectionDecider :: Ghost -> Direction
ghostDirectionDecider g | gstate == Run   = undefined --implement diff move algorithms
                        | gstate == Dead  = undefined
                        | gtype == Blinky = undefined
                        | gtype == Pinky  = undefined
                        | gtype == Clyde  = undefined
                        | otherwise       = undefined --inky
  where
    gstate = ghostState g
    gtype = ghostType g

-- | Update the collision for the game state
collision :: GameState -> GameState
collision gstate = ghostCollisions $ fieldCollision gstate

-- | Given a gamestate handle the collision for pacman with fields
fieldCollision :: GameState -> GameState
fieldCollision gstate_@(GameState {grid = grid_, pacman = pacman_@(Pac {pacPos = pacPos_}), ghosts = ghosts_,
                        score = score_@(Sc {currScore = currScore_})}) 
  = gstate_ { grid = newGrid, ghosts = updateGhostForPowerPellet power ghosts_, score = score_ {currScore = newScore}}
    where 
      (newGrid, newScore, power) = fieldCollisionHelper (gamePosToGridPos pacPos_) grid_ currScore_

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
makeGhostRun ghost@(Gho {ghostState = ghostState_}) | ghostState_ == Normal = ghost{ ghostState = Run} 
                                                    | otherwise             = ghost

-- | Return the grid position for a given game position
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
ghostCollisions gstate_@(GameState {pacman = pacman_@(Pac {pacPos = pacPos_, pacLives = pacLives_}), ghosts = ghosts_, state = state_}) 
    | pacmanHurt = resetLvl gstate_
    | otherwise  = gstate_ {ghosts = newGhosts}
    where 
      newGhosts = map fst ghostBools
      ghostBools = map (handleGhostCollison . checkGhostCollision pacPos_) ghosts_
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
                     | state gstate == Ended && highScore (score gstate) > currScore (score gstate) = do 
                                                                                                        putStr "writing highscore"
                                                                                                        writeFile "./Txtfiles/HighScore.txt" (show (currScore (score gstate))) 
                                                                                                        return gstate 
                     |otherwise = return gstate
  where timeToStart = 5



-- | Spawning randomly a cherry at a random spot.
spawnCherry :: GameState -> GameState
spawnCherry gstate | randomNumber == 1 = gstate {grid = cherryInserter newGen $ grid gstate, rng = newGen }
                   | otherwise = gstate{rng = newGen}
  where generator = rng gstate
        (randomNumber, newGen) = randomR (1, cherrySpawnChance) generator  :: (Int, StdGen)
        cherrySpawnChance = 250 --1/chance so the higher this number the lower the chance

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
