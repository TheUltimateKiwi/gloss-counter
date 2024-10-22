module View where
import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = viewPure


viewPure :: GameState -> IO Picture
viewPure gameState@GameState {infoToShow = a, elapsedTime = b} = do
    mappy <- giveBitMap "map"
    pacman <- giveBitMap "Pac-Man"
    let pacManWithMap = Pictures [mappy]  -- Adjust (x, y) as needed
    return pacManWithMap
      



giveBitMap :: String -> IO Picture
giveBitMap "map"     = loadBMP "./Pictures/Map.bmp"
giveBitMap "Pac-Man" = loadBMP "./Pictures/Pacman-Basic.bmp"