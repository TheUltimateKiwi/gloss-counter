module View where
import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gs = return (viewPure gs)


viewPure :: GameState -> Picture
viewPure gameState@GameState {grid = grid_ ,pacman = Pac {pacPos = (c,d)},score = Sc {currScore = score_, highScore = highScore_}, ghosts = ghosts_, sprites = [pacman, pellet, empty, cherry, power, wall, red_, blue_, pink_, yellow_]} =
 
    let gridPicture = Pictures $ map (\sq -> squareToPic sq [pellet,empty,power,cherry,wall]) grid_

        ghostsPicture= Pictures [translate (-20) 0 yellow_,translate (-40) 0 red_,translate (-60) 0 blue_,translate (-80) 0 pink_]
        scoretxt     = Pictures [translate (-40) 80 $ scale 0.3 0.3 $ color white $ Text ("Score  " ++ show score_), highscoretxt]
        highscoretxt = translate (-40) 40 $ scale 0.15 0.15 $ color white $ Text ("HighScore  " ++ show highScore_)

        fullPicture  = Pictures [gridPicture, ghostsPicture, translate c d pacman, scoretxt]
    in fullPicture

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


