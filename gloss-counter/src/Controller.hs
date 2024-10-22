module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do randomNumber <- randomIO
       let newNumber = abs randomNumber `mod` 10
       return $ GameState 
        (ShowANumber newNumber) 
        [((0,0),Empty)] 
        (Pac { pacPos = (0,0), pacDir = N, pacDesDir = E, pacLives = 3}) 
        [(Gho {ghostPos = (0,0), ghostDir = N, ghostType = Blinky, ghostState = Normal})] 
        (Sc {currScore = 0, highScoreName = "Bob", highScore = 200}) 
        Paused 
        (mkStdGen 69) 
        0
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  = gstate { infoToShow = ShowAChar c } -- If the user presses a character key, show that one
inputKey (EventKey (SpecialKey c) _ _ _) gstate@GameState{infoToShow = q, pacman = (Pac {pacPos = (x,y),pacDir = a, pacDesDir = b, pacLives = d}), elapsedTime = qq} 
  | c == KeyLeft = gstate { pacman = (Pac {pacPos = (x-5,y),pacDir = a, pacDesDir = b, pacLives = d})} 
  | c == KeyRight = gstate { pacman = (Pac {pacPos = (x+5,y),pacDir = a, pacDesDir = b, pacLives = d})} 
  | c == KeyUp = gstate  { pacman = (Pac {pacPos = (x,y+5),pacDir = a, pacDesDir = b, pacLives = d})} 
  | c == KeyDown = gstate { pacman = (Pac {pacPos = (x,y-5),pacDir = a, pacDesDir = b, pacLives = d})} 
inputKey _ gstate = gstate -- Otherwise keep the same.