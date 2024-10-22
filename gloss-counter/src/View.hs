module View where
import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = viewPure


viewPure :: GameState -> IO Picture
viewPure gameState@GameState {pacman = (Pac {pacPos = (c,d),pacDir = _, pacDesDir = _, pacLives = _})} = do
    mappy <- giveBitMap "map"
    pacman <- giveBitMap "Pac-Man"
    let pacManWithMap = Pictures [mappy, translate c d pacman]  -- Adjust (x, y) as needed
    return pacManWithMap
      
      



giveBitMap :: String -> IO Picture
giveBitMap "map"     = loadBMP "./Pictures/Map.bmp"
giveBitMap "Pac-Man" = loadBMP "./Pictures/Pacman-Basic.bmp"