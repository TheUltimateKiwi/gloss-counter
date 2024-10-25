{-# LANGUAGE BlockArguments #-}
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = do
  return $ movement gstate


-- | Update the movement for the game state
movement :: GameState -> GameState
movement gstate = 
         gstate { pacman = pacMovement' $ pacman gstate, ghosts = ghostMovement $ ghosts gstate}
            
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
ghostDirectionDecider g = case ghostState g of 
  Dead ->            undefined -- implement the different algorithms
  Run ->             undefined
  Normal -> case ghostType g of 
          Blinky ->  undefined
          Pinky ->   undefined
          Inky ->    undefined
          Clyde ->   undefined 




-- | Update the collision for the game state
collision :: GameState -> GameState
collision = undefined

-- | Regulate the state for the game state
regulateState :: GameState -> GameState
regulateState = undefined

spawnCherry :: GameState -> GameState
spawnCherry gstate = undefined 
  where generator = rng gstate




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

