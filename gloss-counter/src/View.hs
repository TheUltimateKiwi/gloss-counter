module View where
import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view gs = return (viewPure gs)


viewPure :: GameState -> Picture
viewPure gstate@GameState {
    
    pacman = Pac {pacPos = (c,d), pacLives = lives_},
    score = Sc {currScore = score_,
    highScore = highScore_},
    sprites = [pacman, pellet, empty, cherry, power, wall, red_, blue_, pink_, yellow_, weak, dead],
    state = state_ } =

    let gridPicture = Pictures $ map (\sq -> squareToPic sq [pellet,empty,power,cherry,wall]) (grid gstate)
    
        pacPicture = translate gridoffsetX gridoffsetY $ translate c d pacman
        ghostsPicture = Pictures $ map (\gh -> ghostToPic gh [red_,blue_,pink_,yellow_, weak, dead]) (ghosts gstate)
        scoretxt     = Pictures [translate (-100) 240 $ scale 0.3 0.3 $ color white $ Text ("Score  " ++ show score_), highscoretxt]
        highscoretxt = translate (-80) 200 $ scale 0.15 0.15 $ color white $ Text ("HighScore  " ++ show highScore_)
        livestxt = translate (-20) (-290) $ scale 0.1 0.1 $ color white $ Text ("Lives: " ++ show lives_)

        statetxt| state_ == Playing = Pictures []
                | state_ == Paused = Pictures [transparentGreyOverlay, translate (-160) 0 $ scale 0.65 0.65 $ text "PAUSED" ]
                | state_ == Starting = Pictures [transparentGreyOverlay, translate (-160) 0 $ scale 0.35 0.35 $ text ("Starting in " ++ show (round (3 - elapsedTime gstate))) ]
                | state_ == Ended = Pictures [transparentGreyOverlay, translate (-200) 0 $ scale 0.2 0.2 $ text "You Lost Press Enter To Restart" ]
            where transparentGreyOverlay = scale 5 5 $ color (makeColor 0.8 0.8 0.8 0.8) $ polygon $ rectanglePath 340 340

        fullPicture  = Pictures [gridPicture, ghostsPicture, pacPicture, scoretxt, statetxt, livestxt]

    in fullPicture

squareToPic :: Square -> [Picture] -> Picture
squareToPic ((x,y),field_) pics = translate gridoffsetX gridoffsetY $ translate (x*20) (y*(-20)) (pics !! fieldToPic field_)

gridoffsetX :: Float
gridoffsetX = -200
gridoffsetY :: Float
gridoffsetY = 160

fieldToPic :: Field -> Int
fieldToPic Pellet = 0
fieldToPic Empty = 1
fieldToPic Power = 2
fieldToPic Cherry = 3
fieldToPic Wall = 4

ghostToPic :: Ghost -> [Picture] -> Picture
ghostToPic gho pics = translate gridoffsetX gridoffsetY $ translate x y $ pics !! ghostTypeToPic (ghostType gho) (ghostState gho)
    where (x,y) = ghostPos gho

ghostTypeToPic :: GhostType -> GhostState -> Int
ghostTypeToPic _ Dead  = 5
ghostTypeToPic _ Run   = 4
ghostTypeToPic Blinky _= 0
ghostTypeToPic Inky _  = 1
ghostTypeToPic Pinky _ = 2
ghostTypeToPic Clyde _ = 3

