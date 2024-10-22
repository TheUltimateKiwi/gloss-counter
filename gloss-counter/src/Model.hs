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
 [
    ((0, 0), Wall),   ((0, 1), Wall),   ((0, 2), Wall),   ((0, 3), Wall),   ((0, 4), Wall),   ((0, 5), Wall),   ((0, 6), Wall),
    ((1, 0), Wall),   ((1, 1), Pellet), ((1, 2), Empty),  ((1, 3), Empty),  ((1, 4), Pellet), ((1, 5), Fruit),  ((1, 6), Wall),
    ((2, 0), Wall),   ((2, 1), Empty),  ((2, 2), Cherry), ((2, 3), Empty),  ((2, 4), Empty),  ((2, 5), Pellet), ((2, 6), Wall),
    ((3, 0), Wall),   ((3, 1), Empty),  ((3, 2), Empty),  ((3, 3), Empty),  ((3, 4), Pellet), ((3, 5), Empty),  ((3, 6), Wall),
    ((4, 0), Wall),   ((4, 1), Pellet), ((4, 2), Empty),  ((4, 3), Fruit),  ((4, 4), Cherry), ((4, 5), Empty),  ((4, 6), Wall),
    ((5, 0), Wall),   ((5, 1), Wall),   ((5, 2), Wall),   ((5, 3), Wall),   ((5, 4), Wall),   ((5, 5), Wall),   ((5, 6), Wall)
    ]
  (Pac { pacPos = (0,0), pacDir = N, pacDesDir = E, pacLives = 3}) 
  [(Gho {ghostPos = (0,0), ghostDir = N, ghostType = Blinky, ghostState = Normal})] 
  (Sc {currScore = 0, highScoreName = "Bob", highScore = 200}) 
  Paused 
  (mkStdGen 69) 
  0
  
  