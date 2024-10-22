module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = do return gstate
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey c) _ _ _) gstate@GameState{pacman = (Pac {pacPos = (x,y),pacDir = a, pacDesDir = b, pacLives = d}), elapsedTime = qq} 
  | c == KeyLeft = gstate { pacman = (Pac {pacPos = (x-5,y)})} 
  | c == KeyRight = gstate { pacman = (Pac {pacPos = (x+5,y)})} 
  | c == KeyUp = gstate  { pacman = (Pac {pacPos = (x,y+5)})} 
  | c == KeyDown = gstate { pacman = (Pac {pacPos = (x,y-5)})} 
inputKey _ gstate = gstate -- Otherwise keep the same.