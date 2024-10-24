module View where
import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = viewPure


viewPure :: GameState -> IO Picture
viewPure gameState@GameState {grid = grid_ ,pacman = Pac {pacPos = (c,d),pacDir = _, pacDesDir = _, pacLives = _},score = Sc {currScore = score_, highScore = highScore_}} = do
    pacman <- giveBitMap "Pac-Man"
    pellet <- giveBitMap "pellet"
    empty <- giveBitMap "empty"
    cherry <- giveBitMap "cherry"
    power <- giveBitMap "power"
    wall <- giveBitMap "wall"
    red <- giveBitMap "red"
    
    

    let pacManWithMap = Pictures [gridToPic grid_ [pellet,empty,cherry,power,wall], translate c d pacman, scoretxt, highscoretxt]  -- Adjust (x, y) as needed
    return pacManWithMap
    where
        
        scoretxt = translate (-40) 180 $ scale 0.3 0.3 $ color white $ Text ("Score  " ++ show score_)
        highscoretxt= translate (-40) 140 $ scale 0.15 0.15 $ color white $ Text ("HighScore  " ++ show highScore_)


gridToPic :: Grid -> [Picture] -> Picture
gridToPic [((x,y),_)] pel = Pictures [translate (x*20) (y*20) (pel !! 4)]
gridToPic (((x,y),Pellet):xs) pel = Pictures [translate (x*20) (y*20) (head pel), gridToPic xs pel]
gridToPic (((x,y),Empty):xs) pel = Pictures [translate (x*20) (y*20) (pel !! 1), gridToPic xs pel]
gridToPic (((x,y),Power):xs) pel = Pictures [translate (x*20) (y*20) (pel !! 2), gridToPic xs pel]
gridToPic (((x,y),Cherry):xs) pel = Pictures [translate (x*20) (y*20) (pel !! 3), gridToPic xs pel]
gridToPic (((x,y),Wall):xs) pel = Pictures [translate (x*20) (y*20) (pel !! 4), gridToPic xs pel]


giveBitMap :: String -> IO Picture
giveBitMap "Pac-Man" = loadBMP "./Pictures/Pacman-Basic.bmp"
giveBitMap "pellet" = loadBMP "./Pictures/pellet_tile.bmp"
giveBitMap "empty"     = loadBMP "./Pictures/empty_tile.bmp"
giveBitMap "cherry" = loadBMP "./Pictures/cherry_tile.bmp"
giveBitMap "power" = loadBMP "./Pictures/powerpellet_tile.bmp"
giveBitMap "wall"     = loadBMP "./Pictures/wall_tile.bmp"
giveBitMap "red" = loadBMP "./Pictures/red_ghost_tile.bmp"
