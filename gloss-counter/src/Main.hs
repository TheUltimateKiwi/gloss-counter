module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss (loadBMP)

main :: IO ()
main = 
    
    do 
        startState <- initialState
        playIO (InWindow "Counter" (340, 340) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              startState       -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

