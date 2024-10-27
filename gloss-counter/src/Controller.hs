{-# LANGUAGE BlockArguments #-}
module Controller where
import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | state gstate == Playing = return $ spawnCherry $ fieldCollision $ movement gstate
                 | otherwise = return $ regulateState gstate { elapsedTime = elapsedTime gstate + secs }  --If gameplay is paused or Ended no need to do movement or other things




-- | Update the movement for the game state
movement :: GameState -> GameState
movement gstate = gstate { pacman = pacMovement' $ pacman gstate}--, ghosts = ghostMovement $ ghosts gstate}

pacMovement' :: Pacman -> Pacman  -- | Pacman movement for each step
pacMovement' pacman_@(Pac {pacPos = pacPos, pacDir = currDir, pacDesDir = desDir})
  | validDirection pacPos desDir  = pacman_ { pacPos = goToDirection pacPos desDir, pacDir = desDir}
  | validDirection pacPos currDir = pacman_ { pacPos = goToDirection pacPos currDir}
  | otherwise                     = pacman_ { pacDir = X }

goToDirection :: Point -> Direction -> Point
goToDirection (x, y) dir | dir == N = (x        , y + speed)
                         | dir == E = (x + speed, y        )
                         | dir == S = (x        , y - speed)
                         | dir == W = (x - speed, y        )
                         | dir == X = (x        , y        )
                            where
                              speed = 2


validDirection :: Point -> Direction -> Bool
validDirection pos dir = True -- implementation of not going through walls.

  -- | GHOST movement for each step
ghostMovement :: [Ghost] -> [Ghost]
ghostMovement = map singularGhostMovement

singularGhostMovement :: Ghost -> Ghost
singularGhostMovement ghost_@(Gho {ghostPos = ghopos, ghostDir = ghodir})
  | isAtCrossRoad ghopos = let decidedDir = ghostDirectionDecider ghost_
                            in ghost_ {ghostPos = goToDirection ghopos decidedDir, ghostDir = decidedDir}
  | otherwise                = ghost_ {ghostPos = goToDirection ghopos ghodir}

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

fieldCollision :: GameState -> GameState
fieldCollision gstate_@(GameState {grid = grid_, pacman = pacman_@(Pac {pacPos = pacPos_}), ghosts = ghosts_,
                        score = score_@(Sc {currScore = currScore_})}) 
  = gstate_ { grid = newGrid, ghosts = updateGhostForPowerPellet power ghosts_, score = score_ {currScore = newScore}}
    where 
      (newGrid, newScore, power) = fieldCollisionHelper (gamePosToGridPos pacPos_) grid_ currScore_
      
updateGhostForPowerPellet :: Bool -> [Ghost] -> [Ghost]
updateGhostForPowerPellet False ghosts = ghosts 
updateGhostForPowerPellet True ghosts = map makeGhostRun ghosts

makeGhostRun :: Ghost -> Ghost
makeGhostRun ghost@(Gho {ghostState = ghostState_}) | ghostState_ == Normal = ghost{ ghostState = Run} 
                                                    | otherwise             = ghost

fieldCollisionHelper :: Point -> Grid -> Int -> (Grid, Int, Bool)
fieldCollisionHelper pos grid@(square@(fieldPos, field): restGrid) oldScore | pos == fieldPos = (newSquare : restGrid, newScore, power)
                                                                            | otherwise       = (square : recursionGrid, recursionScore, False)
                                                                              where
                                                                                (newSquare, newScore, power) = squarePacmanCollision square oldScore
                                                                                (recursionGrid, recursionScore, False) = fieldCollisionHelper pos restGrid oldScore

squarePacmanCollision :: Square -> Int -> (Square, Int, Bool)
squarePacmanCollision square@(fieldPos, field) score | field == Pellet = ((fieldPos, Empty), score + 10, False)
                                                     | field == Power  = ((fieldPos, Empty), score + 100, True)
                                                     | field == Cherry = ((fieldPos, Empty), score + 500, False)
                                                     | otherwise = (square, score, False)


gamePosToGridPos :: Point -> Point
gamePosToGridPos (0, 0) = (0                  , 0)
gamePosToGridPos (x, 0) = (roundFloat (x / 20), 0)
gamePosToGridPos (0, y) = (0                  , roundFloat (-y / 20))
gamePosToGridPos (x, y) = (roundFloat (x / 20), roundFloat (-y / 20))

roundFloat :: Float -> Float
roundFloat x = fromIntegral $ round x

ghostCollisions :: GameState -> GameState
ghostCollisions gstate_@(GameState {pacman = pacman_@(Pac {pacPos = pacPos_, pacLives = pacLives_}), ghosts = ghosts_, state = state_}) = 
  gstate_ {pacman = pacman_{pacLives = newLives}, ghosts = newGhosts, state = newState}
    where 
      newState  = undefined 
      newGhosts = undefined
      newLives  = undefined

handleGhostCollison :: (Ghost, Bool) -> (Ghost, Bool)
handleGhostCollison (ghost, False) = (ghost, False) 
handleGhostCollison (ghost@(Gho{ghostState = Run}), True) = (ghost{ ghostState = Dead}, False)
handleGhostCollison (ghost@(Gho{ghostState = Normal}), True) = (ghost, True)
handleGhostCollison (ghost@(Gho{ghostState = Dead}), True) = (ghost, False)

checkGhostCollision :: Point -> Ghost -> (Ghost, Bool)
checkGhostCollision (xP, yP) ghost@(Gho{ghostPos = (xG,yG)}) | (xP > xG && xP - 20 > xG) || (xP < xG && xP + 20 < xG)
                                                                || (yP > yG && yP - 20 > yG) || (yP < yG && yP + 20 < yG) = (ghost, False)
                                                             | otherwise = (ghost, True)

-- | Regulate the state for the game state
regulateState :: GameState -> GameState
regulateState gstate | state gstate == Starting && elapsedTime gstate >= timeToStart = gstate { state = Playing }
                     | otherwise = gstate
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
               | pausedOrEnded == Ended = inputNewGame e gstate
               | otherwise = return (inputKey e gstate) --Playing && Starting
  where pausedOrEnded = state gstate

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey c) Down _ _) gstate@GameState{ pacman = pacman_ }
  | c == KeySpace = gstate { state = Paused }
    | c == KeyEnter = gstate { state = Ended }
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
  | c == KeyEnter = followUpState 2 gstate  -- Restart the game !!Random or gstate Int to keep track
inputNewGame _ gstate = return gstate -- Otherwise keep the same.