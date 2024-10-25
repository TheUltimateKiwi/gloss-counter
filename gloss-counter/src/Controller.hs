{-# LANGUAGE BlockArguments #-}
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return $ spawnCherry $ movement gstate


-- | Update the movement for the game state
movement :: GameState -> GameState
movement gstate =
         gstate { pacman = pacMovement' $ pacman gstate}--, ghosts = ghostMovement $ ghosts gstate}

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
isAtCrossRoad point = undefined-- implement checking if the ghost is at a crossroad

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
regulateState = undefined

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

findSq :: Int -> Field -> [Square] -> Square
findSq _ _ [] = ((0,0),Empty)-- ???
findSq 0 f ((x,_):xs) = (x,f)
findSq i f (x:xs) = findSq (i - 1) f xs

replaceAt' :: Square -> [Square] -> [Square]
replaceAt' _ [] = []
replaceAt' sq@((x,y),f) (ff@((x',y'),_):xs) | x == x' && y == y' = sq : xs
                                            | otherwise = ff : replaceAt' sq xs


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey c) _ _ _) gstate@GameState{pacman = (Pac {pacPos = (x,y),pacDir = a, pacDesDir = b, pacLives = d}), elapsedTime = qq}
  | c == KeyLeft  = gstate { pacman = (Pac {pacPos = (x,y),pacDir =a, pacDesDir = W, pacLives = d})}
  | c == KeyRight = gstate { pacman = (Pac {pacPos = (x,y),pacDir =a, pacDesDir = E, pacLives = d})}
  | c == KeyUp    = gstate { pacman = (Pac {pacPos = (x,y),pacDir =a, pacDesDir = N, pacLives = d})}
  | c == KeyDown  = gstate { pacman = (Pac {pacPos = (x,y),pacDir =a, pacDesDir = S, pacLives = d})}
inputKey _ gstate = gstate -- Otherwise keep the same.

