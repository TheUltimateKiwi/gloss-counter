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
    red <- giveBitMap "red"

    -- stringforgrid <- readFile ""


    let pacManWithMap = Pictures [Pictures $ map (\sq -> squareToPic sq [pellet,empty,cherry,power,wall]) grid_, translate c d pacman, scoretxt, highscoretxt]  -- Adjust (x, y) as needed
    return pacManWithMap
    where
        scoretxt     = translate (-40) 180 $ scale 0.3 0.3 $ color white $ Text ("Score  " ++ show score_)
        highscoretxt = translate (-40) 140 $ scale 0.15 0.15 $ color white $ Text ("HighScore  " ++ show highScore_)

squareToPic :: Square -> [Picture] -> Picture
squareToPic ((x,y),field_) pics = translate (x*20) (y*20) (pics !! fieldToPic field_)

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

stringToGrid :: String -> Grid
stringToGrid str = _indexfieldlsit
 where
    _indexfieldlsit = map smth $ concat sindexfieldlist
    sindexfieldlist = map (zip [0..]) findexfieldlist
    findexfieldlist = map (zip [0..]) fieldlist
    fieldlist = map (map charToField) linestr
    linestr = lines str

smth:: (Float,(Float, Field)) -> Square
smth (y,(x,f)) = ((x,y),f)

charToField :: Char -> Field
charToField 'X' = Wall
charToField 'E' = Empty
charToField '.' = Pellet
charToField 'S' = Power
charToField 'C' = Cherry

