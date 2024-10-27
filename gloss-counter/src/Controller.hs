{-# LANGUAGE BlockArguments #-}
module Controller where
import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate | state gstate == Playing = return $ spawnCherry $ movement gstate
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
goToDirection (x, y) N = (x    , y + 1)
goToDirection (x, y) E = (x + 1, y    )
goToDirection (x, y) S = (x    , y - 1)
goToDirection (x, y) W = (x - 1, y    )
goToDirection (x, y) X = (x    , y    )

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
collision = undefined

-- | Regulate the state for the game state
regulateState :: GameState -> GameState
regulateState gstate | state gstate == Starting && elapsedTime gstate >= timeToStart = gstate { state = Playing }
                     | otherwise = gstate
  where timeToStart = 3

-- | Spawning randomly a cherry at a random spot.
spawnCherry :: GameState -> GameState
spawnCherry gstate | randomNumber == 1 = gstate {grid = cherryInserter newGen $ grid gstate, rng = newGen }
                   | otherwise = gstate{rng = newGen}
  where generator = rng gstate
        (randomNumber, newGen) = randomR (1, 75) generator  :: (Int, StdGen)

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
               | pausedOrEnded == Ended = return (inputNewGame e gstate)
               | otherwise = return (inputKey e gstate) --Playing && Starting
  where pausedOrEnded = state gstate

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey c) Down _ _) gstate@GameState{ pacman = pacman_ }
  | c == KeySpace = gstate { state = Paused }
  | c == KeyLeft  = gstate { pacman = pacman_ {pacDesDir = W}}
  | c == KeyRight = gstate { pacman = pacman_ {pacDesDir = E}}
  | c == KeyUp    = gstate { pacman = pacman_ {pacDesDir = N}}
  | c == KeyDown  = gstate { pacman = pacman_ {pacDesDir = S}}
inputKey _ gstate = gstate -- Otherwise keep the same.

inputUnPause :: Event -> GameState -> GameState
inputUnPause (EventKey (SpecialKey c) Down _ _) gstate
  | c == KeySpace = gstate { state = Playing }   -- Unpause the game
inputUnPause _ gstate = gstate -- Otherwise keep the same.

inputNewGame :: Event -> GameState -> GameState
inputNewGame (EventKey (SpecialKey c) Down _ _) gstate
  | c == KeyEnter = gstate { state = Starting }  -- Restart the game
inputNewGame _ gstate = gstate -- Otherwise keep the same.