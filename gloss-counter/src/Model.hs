module Model where
import Graphics.Gloss (loadBMP, Picture, Point)

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowAPicture Picture

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 30

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , pacPos :: Point
                 , elapsedTime :: Float
                 }

initialState :: Picture -> GameState
initialState a = GameState (ShowAPicture a) (0,0) 0
  
  