module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = do return $ spawnCherry $ regulateState $ collision $ movement gstate
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Update the movement for the game state
movement :: GameState -> GameState
movement gstate@GameState{pacman = _pacman, ghosts = _ghosts} = 
         gstate{pacman = (Pac {pacPos = newPacPos, pacDir = newPacDir}), ghosts = ghostMovement _ghosts}
          where
            (newPacDir, newPacPos) = pacMovement _pacman
            
-- | Pacman movement for each step
pacMovement :: Pacman -> (Direction, Point)
pacMovement pacman@(Pac {pacPos = pacPos@(x,y),pacDir = currDir, pacDesDir = desDir})  
  | validDirection pacPos desDir  = (desDir, goToDirection pacPos desDir)
  | validDirection pacPos currDir = (currDir, goToDirection pacPos desDir)
  | otherwise                     = (X, pacPos)

goToDirection :: Point -> Direction -> Point
goToDirection (x, y) N = (x    , y - 1)
goToDirection (x, y) E = (x + 1, y    )
goToDirection (x, y) S = (x    , y + 1)
goToDirection (x, y) W = (x - 1, y    )
goToDirection (x, y) X = (x    , y    )

validDirection :: Point -> Direction -> Bool
validDirection pos dir = True

-- | Ghost movement for each step
ghostMovement :: [Ghost] -> [Ghost]
ghostMovement ghost = ghost -- not yet implemented

-- | Update the collision for the game state
collision ::GameState -> GameState
collision = undefined

-- | Regulate the state for the game state
regulateState :: GameState -> GameState
regulateState = undefined

spawnCherry :: GameState -> GameState
spawnCherry = undefined


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey c) _ _ _) gstate@GameState{pacman = (Pac {pacPos = (x,y),pacDir = a, pacDesDir = b, pacLives = d}), elapsedTime = qq} 
  | c == KeyLeft = gstate { pacman = (Pac {pacDesDir = W})} 
  | c == KeyRight = gstate { pacman = (Pac {pacDesDir = E})} 
  | c == KeyUp = gstate  { pacman = (Pac {pacDesDir = N})} 
  | c == KeyDown = gstate { pacman = (Pac {pacDesDir = S})} 
inputKey _ gstate = gstate -- Otherwise keep the same.

