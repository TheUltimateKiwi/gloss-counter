module View where
import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = viewPure


viewPure :: GameState -> IO Picture
viewPure gameState@GameState {grid = grid_ ,pacman = Pac {pacPos = (c,d)},score = Sc {currScore = score_, highScore = highScore_}, ghosts = ghosts_} = do
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

    stringforgrid <- readFile "./Txtfiles/PacmanGrid1.txt"
    let gridPicture = Pictures $ map (\sq -> squareToPic sq [pellet,empty,power,cherry,wall]) $ stringToGrid stringforgrid
    let ghostsPicture = Pictures [translate (-20) 0 yellow_,translate (-40) 0 red_,translate (-60) 0 blue_,translate (-80) 0 pink_]


    let pacManWithMap = Pictures [gridPicture, translate c d pacman, ghostsPicture, scoretxt]
    
    return pacManWithMap
    
    where
        scoretxt     = Pictures [translate (-40) 80 $ scale 0.3 0.3 $ color white $ Text ("Score  " ++ show score_), highscoretxt]
        highscoretxt = translate (-40) 40 $ scale 0.15 0.15 $ color white $ Text ("HighScore  " ++ show highScore_)

squareToPic :: Square -> [Picture] -> Picture
squareToPic ((x,y),field_) pics = translate (x*20) (y*(-20)) (pics !! fieldToPic field_)

fieldToPic :: Field -> Int
fieldToPic Pellet = 0
fieldToPic Empty = 1
fieldToPic Power = 2
fieldToPic Cherry = 3
fieldToPic Wall = 4

ghostToPic :: [Ghost] -> [Picture] -> Picture
ghostToPic (x:xs) pics = undefined

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
    _indexfieldlsit = map smth sindexfieldlist
    sindexfieldlist = zip [0..] findexfieldlist
    findexfieldlist = map (zip [0..]) fieldlist
    fieldlist = map (map charToField) linestr
    linestr = lines str

smth:: (Float,[(Float, Field)]) -> [Square]
smth (y,(x,f):[]) = [((x,y),f)]
smth (y,(x,f):xs) = ((x,y),f) : smth (y,xs)  

charToField :: Char -> Field
charToField 'X' = Wall
charToField 'E' = Empty
charToField '.' = Pellet
charToField 'S' = Power
charToField 'C' = Cherry
charToField c = Wall

