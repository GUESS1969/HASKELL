-- Module for rendering game state in the CLI.
module GameRenderCLI where

import GameTypes ( Game(..), Ball(..), Rect(..), Racket(..), Brick(..), GameStatus(..) )

-- Constants
screenWidth :: Int
screenWidth = 45

screenHeight :: Int
screenHeight = 24

-- Function to print the current game state
printGameState :: Game -> String
printGameState game = 
    border ++
    concatMap printRow [0..(screenHeight-1)] ++
    border
  where
    border = replicate screenWidth '-' ++ "\n"
    printRow y = do
        [ if hasBall x y then '0'
          else if hasRacket x y then '='
          else if hasBrick x y then '#'
          else ' ' | x <- [0..(screenWidth-1)] ]  
        ++ "\n"

    hasBall x y = rectsCollide (Rect x y 1 1) (ballRect (gameBall game))
    hasRacket x y = rectsCollide (Rect x y 1 1) (racketRect (gameRacket game))
    hasBrick x y = any (\brick -> rectsCollide (Rect x y 1 1) (brickRect brick)) (gameBricks game)

rectsCollide :: Rect -> Rect -> Bool
rectsCollide a b = 
    rectX a < rectX b + rectWidth b &&
    rectX a + rectWidth a > rectX b &&
    rectY a < rectY b + rectHeight b &&
    rectY a + rectHeight a > rectY b