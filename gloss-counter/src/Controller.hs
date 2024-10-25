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
movement gstate@GameState{pacman = _pacman} = 
         gstate{pacman = (Pac {pacPos = newPacPos, pacDir = newPacDir})}
          where
            (newPacDir, newPacPos) = pacMovement _pacman
            

pacMovement :: Pacman -> (Direction, Point)
pacMovement pacman@(Pac {pacPos = pacPos@(x,y),pacDir = currDir, pacDesDir = desDir}) = undefined -- Check if it is possible for pacman to go to the desDir if not do the currDir,
                                               -- Check if do currDir if it is possible for pacman if not do direction X.
                                               -- with the (new) direction increment the pacPos and return the new direction and point.

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

