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

data State = Starting | Paused | Playing | Ended deriving Eq 
data Pacman = Pac {pacPos :: Point, pacDir:: Direction, pacDesDir:: Direction, pacLives :: Int}
data Direction = N | E | S | W | X deriving Eq
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
  let grid_ = stringToGrid stringforgrid
  spritesPure <- loadSprites
  
  generator <- newStdGen

  high_score <- readFile "./Txtfiles/HighScore.txt"
  
  return (GameState{
    grid = grid_,
    pacman = (Pac { pacPos = (10*20,-16*20), pacDir = X, pacDesDir = X, pacLives = 3}),
    ghosts = basicGhosts,
    score = (Sc {currScore = 0, highScore = read high_score}), --write highscore on death??
    state = Starting,
    rng = generator,
    elapsedTime = 0,
    sprites = spritesPure
  })    

basicGhosts :: [Ghost]
basicGhosts =  [(Gho {ghostPos = (8*20,-10*20), ghostDir = X, ghostType = Blinky, ghostState = Normal}),
                (Gho {ghostPos = (9*20,-10*20), ghostDir = X, ghostType = Inky, ghostState = Normal}),
                (Gho {ghostPos = (11*20,-10*20), ghostDir = X, ghostType = Pinky, ghostState = Normal}),
                (Gho {ghostPos = (12*20,-10*20), ghostDir = X, ghostType = Clyde, ghostState = Normal})]

followUpState :: Int -> GameState -> IO GameState
followUpState int_ gstate = do 
  let grid_filepath | int_ == 1 = "./Txtfiles/PacmanGrid1.txt" --choose the grid based on what initialstate is chosen.
                    | int_ == 2 = "./Txtfiles/PacmanGrid2.txt"
                    | int_ == 3 = "./Txtfiles/PacmanGrid3.txt"
  stringforgrid <- readFile grid_filepath
  let newgrid_ = stringToGrid stringforgrid 

  return (gstate {
    grid = newgrid_,  --grid has now changed to a new one.
    pacman = (pacman gstate) {pacPos = (10*20,-16*20), pacDir = X, pacDesDir = X}, --pacman lives stay consistant
    ghosts = basicGhosts, -- ghosts are reset
    state = Starting,
    elapsedTime = 0
  }) 

resetLvl :: GameState -> GameState
resetLvl gstate | pacLives (pacman gstate) <= 0 = gstate {state = Ended} -- Check if you are out of lives.
                | otherwise = (gstate {
      pacman = (pacman gstate) {pacPos = (10*20,-16*20), pacDir = X, pacDesDir = X, pacLives = pacLives (pacman gstate) - 1}, --pacman lives stay are lowered by one
      ghosts = basicGhosts, -- ghosts are reset
      state = Starting,
      elapsedTime = 0
    })
-- score should carry over here and highscore is consistant, same for gen, sprites and grid

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
    weak <- giveBitMap "weak"
    dead <- giveBitMap "dead"
    return [pacman, pellet, empty, cherry, power, wall, red_, blue_, pink_, yellow_, weak, dead]
  

giveBitMap :: String -> IO Picture
giveBitMap "Pac-Man"= loadBMP "./Pictures/Pacman-Basic.bmp"
giveBitMap "pellet" = loadBMP "./Pictures/pellet_tile.bmp"
giveBitMap "empty"  = loadBMP "./Pictures/empty_tile.bmp"
giveBitMap "cherry" = loadBMP "./Pictures/cherry_tile.bmp"
giveBitMap "power"  = loadBMP "./Pictures/powerpellet_tile.bmp"
giveBitMap "wall"   = loadBMP "./Pictures/wall_tile.bmp"
giveBitMap "red"    = loadBMP "./Pictures/red_ghost_tile.bmp"
giveBitMap "blue"   = loadBMP "./Pictures/blue_ghost.bmp"
giveBitMap "yellow" = loadBMP "./Pictures/yellow_ghost_tile.bmp"
giveBitMap "pink"   = loadBMP "./Pictures/pink_ghost_tile.bmp"
giveBitMap "weak"   = loadBMP "./Pictures/vulnurable_ghost.bmp"
giveBitMap "dead"   = loadBMP "./Pictures/dead_ghost.bmp"
  
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