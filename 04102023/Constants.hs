module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import System.Exit

-- Constants
title :: String
title = "Bricksbreak"  -- Titre

pathIcon :: FilePath
pathIcon = "images/icon.png"  -- Chemin image icône

winWidth :: Int
winWidth = 480  -- Largeur fenêtre

winHeight :: Int
winHeight = 640  -- Hauteur fenêtre

bricksPerLine :: Int
bricksPerLine = 7  -- Nombre de briques par ligne

bricksPerColumn :: Int
bricksPerColumn = 6  -- Nombre de briques par colonne

nbLives :: Int
nbLives = 3  -- Nombre de vies initiales

pathLife :: FilePath
pathLife = "images/life.png"  -- Chemin image vie

defaultSpeedBX :: Float
defaultSpeedBX = 130  -- Vitesse horizontale

defaultSpeedBY :: Float
defaultSpeedBY = 335  -- Vitesse verticale

pathSoundBrick :: FilePath
pathSoundBrick = "sounds/collision_brick.wav"  -- Chemin son brique

pathSoundRacket :: FilePath
pathSoundRacket = "sounds/collision_racket.wav"  -- Chemin son raquette

pageBeginning :: Int
pageBeginning = 1  -- Page de début

pageRound :: Int
pageRound = 2  -- Page de partie

pageEnd :: Int
pageEnd = 3  -- Page de fin

-- Function to test collision between two rectangles
collideRect :: (Float, Float, Float, Float) -> (Float, Float, Float, Float) -> Bool
collideRect (x1, y1, w1, h1) (x2, y2, w2, h2) =
  x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + h2 && y1 + h1 > y2

--main :: IO ()
--main = do
  --putStrLn title
 -- putStrLn pathIcon
 -- print winWidth
 -- print winHeight
  --print bricksPerLine
 -- print bricksPerColumn
  --print nbLives
  --putStrLn pathLife
  --print defaultSpeedBX
 -- print defaultSpeedBY
 -- putStrLn pathSoundBrick
 -- putStrLn pathSoundRacket
  --print pageBeginning
 -- print pageRound
 -- print pageEnd
