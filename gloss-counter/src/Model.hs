module Model where
import Graphics.Gloss (loadBMP, Picture, Point)
import System.Random (StdGen, mkStdGen, random, randomR)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 30

data GameState = GameState {
                   grid :: Grid
                 , pacman :: Pacman
                 , ghosts :: [Ghost]
                 , score :: Score
                 , state :: State
                 , random :: StdGen
                 , elapsedTime :: Float
                 }

data State = Starting | Paused | Playing | Ended
data Pacman = Pac {pacPos :: Point, pacDir:: Direction, pacDesDir:: Direction, pacLives :: Int}
data Direction = N | E | S | W | X
data Ghost = Gho {ghostPos :: Point, ghostDir:: Direction, ghostType :: GhostType, ghostState :: GhostState}
data GhostType = Blinky | Pinky | Inky | Clyde
data GhostState = Normal | Run | Dead
--The grid will be represented by a list of type Points with their corresponding fields
type Grid = [Square]
type Square = (Point, Field)
data Field = Empty | Pellet | Cherry | Fruit | Wall 
data Fruit = Banana | Apple | BlueBerry | Grape
data Score = Sc {currScore :: Int, highScoreName :: String, highScore :: Int}


initialState :: Picture -> GameState
initialState a = GameState 
  [((0,0),Empty)] 
  (Pac { pacPos = (0,0), pacDir = N, pacDesDir = E, pacLives = 3}) 
  [(Gho {ghostPos = (0,0), ghostDir = N, ghostType = Blinky, ghostState = Normal})] 
  (Sc {currScore = 0, highScoreName = "Bob", highScore = 200}) 
  Paused 
  (mkStdGen 69) 
  0
  
  