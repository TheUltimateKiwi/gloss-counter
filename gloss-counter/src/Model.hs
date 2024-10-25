module Model where
import Graphics.Gloss (loadBMP, Picture, Point, yellow)
import System.Random (StdGen, mkStdGen, random, randomR)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 30

data GameState = GameState {
                   grid :: Grid
                 , pacman :: Pacman
                 , ghosts :: [Ghost]
                 , score :: Score
                 , state :: State
                 , rng :: StdGen
                 , elapsedTime :: Float
                 , sprites :: [Picture]
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
data Field = Empty | Pellet | Power | Cherry | Wall 
instance Show Field where
  show Empty = "."
  show Pellet ="P"
  show Power = "S"
  show Cherry ="C"
  show Wall =  "X"
data Fruit = Banana | Apple | BlueBerry | Grape
data Score = Sc {currScore :: Int, highScore :: Int}


initialState :: IO GameState 
initialState = do 
  
  spritesPure <- loadSprites

  return (GameState{
    grid = [
    ((0, 0), Wall),   ((0, 1), Wall),   ((0, 2), Wall),   ((0, 3), Wall),   ((0, 4), Wall),   ((0, 5), Wall),   ((0, 6), Wall),
    ((1, 0), Wall),   ((1, 1), Pellet), ((1, 2), Empty),  ((1, 3), Empty),  ((1, 4), Pellet), ((1, 5), Cherry), ((1, 6), Wall),
    ((2, 0), Wall),   ((2, 1), Empty),  ((2, 2), Power),  ((2, 3), Empty),  ((2, 4), Empty),  ((2, 5), Pellet), ((2, 6), Wall),
    ((3, 0), Wall),   ((3, 1), Empty),  ((3, 2), Empty),  ((3, 3), Empty),  ((3, 4), Pellet), ((3, 5), Empty),  ((3, 6), Wall),
    ((4, 0), Wall),   ((4, 1), Pellet), ((4, 2), Empty),  ((4, 3), Cherry), ((4, 4), Power),  ((4, 5), Empty),  ((4, 6), Wall),
    ((5, 0), Wall),   ((5, 1), Wall),   ((5, 2), Wall),   ((5, 3), Wall),   ((5, 4), Wall),   ((5, 5), Wall),   ((5, 6), Wall)
    ],
    pacman = (Pac { pacPos = (0,0), pacDir = N, pacDesDir = E, pacLives = 3}),
    ghosts = [(Gho {ghostPos = (0,0), ghostDir = N, ghostType = Blinky, ghostState = Normal})],
    score = (Sc {currScore = 0, highScore = 200}),
    state = Paused,
    rng = mkStdGen 69,
    elapsedTime = 0,
    sprites = spritesPure
  })    
    
loadSprites :: IO [Picture]
loadSprites = do 
    pacman <- giveBitMap "Pac-Man"
    pellet <- giveBitMap "pellet"
    empty <- giveBitMap "empty"
    cherry <- giveBitMap "cherry"
    power <- giveBitMap "power"
    wall <- giveBitMap "wall"
    red_ <- giveBitMap "red"
    blue_ <- giveBitMap "blue"
    pink_ <- giveBitMap "pink"
    yellow_ <- giveBitMap "yellow"

    return [pacman, pellet, empty, cherry, power, wall, red_, blue_, pink_, yellow_]
  

giveBitMap :: String -> IO Picture
giveBitMap "Pac-Man" = loadBMP "./Pictures/Pacman-Basic.bmp"
giveBitMap "pellet" = loadBMP "./Pictures/pellet_tile.bmp"
giveBitMap "empty"     = loadBMP "./Pictures/empty_tile.bmp"
giveBitMap "cherry" = loadBMP "./Pictures/cherry_tile.bmp"
giveBitMap "power" = loadBMP "./Pictures/powerpellet_tile.bmp"
giveBitMap "wall"     = loadBMP "./Pictures/wall_tile.bmp"
giveBitMap "red" = loadBMP "./Pictures/red_ghost_tile.bmp"
giveBitMap "blue" = loadBMP "./Pictures/blue_ghost_tile.bmp"
giveBitMap "yellow" = loadBMP "./Pictures/yellow_ghost_tile.bmp"
giveBitMap "pink" = loadBMP "./Pictures/pink_ghost_tile.bmp"
  