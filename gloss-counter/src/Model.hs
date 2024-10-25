module Model where
import Graphics.Gloss (loadBMP, Picture, Point, yellow)
import System.Random (StdGen, mkStdGen, random, randomR, newStdGen)

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

data GhostType = Blinky | Pinky | Inky | Clyde deriving Eq
data GhostState = Normal | Run | Dead deriving Eq
--The grid will be represented by a list of type Points with their corresponding fields
type Grid = [Square]
type Square = (Point, Field)
data Field = Empty | Pellet | Power | Cherry | Wall deriving Eq

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
  
  stringforgrid <- readFile "./Txtfiles/PacmanGrid1.txt"
  spritesPure <- loadSprites
  generator <- newStdGen
  return (GameState{
    grid = stringToGrid stringforgrid,
    pacman = (Pac { pacPos = (0,0), pacDir = N, pacDesDir = E, pacLives = 3}),
    ghosts = [(Gho {ghostPos = (0,0), ghostDir = N, ghostType = Blinky, ghostState = Normal})],
    score = (Sc {currScore = 0, highScore = 200}),
    state = Paused,
    rng = generator,
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
  
stringToGrid :: String -> [Square]
stringToGrid str = concat _indexfieldlsit
 where
    _indexfieldlsit = map squareTransformer sindexfieldlist
    sindexfieldlist = zip [0..] findexfieldlist
    findexfieldlist = map (zip [0..]) fieldlist
    fieldlist = map (map charToField) linestr
    linestr = lines str

squareTransformer:: (Float,[(Float, Field)]) -> [Square]
squareTransformer (y,[]) = []
squareTransformer (y,(x,f):xs) = ((x,y),f) : squareTransformer (y,xs)

charToField :: Char -> Field
charToField 'X' = Wall
charToField 'E' = Empty
charToField '.' = Pellet
charToField 'S' = Power
charToField 'C' = Cherry
charToField c = Wall