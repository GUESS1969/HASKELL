module Main where

ort System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Reader
import Control.Monad.State
import System.Console.ANSI
import Control.Concurrent
import System.IO
import System.Exit
import qualified Data.ByteString as BS
import Codec.Wav (decodeWav)
--import Constants 
--import Conf 

-- Constants
data Constants = Constants
  { winWidth :: Float
  , winHeight :: Float
  , bricksPerColumn :: Int
  , bricksPerLine :: Int
  , defaultSpeedBy :: Float
  , defaultSpeedBx :: Float
  , nbLives :: Int
  , pathLife :: String
  , pathSoundBrick :: String
  , pathSoundRacket :: String
  } deriving Show

constants :: Constants
constants = Constants
  { winWidth = 800
  , winHeight = 600
  , bricksPerColumn = 5
  , bricksPerLine = 10
  , defaultSpeedBy = 100
  , defaultSpeedBx = 100
  , nbLives = 3
  , pathLife = "Images\\life.png"
  , pathSoundBrick = "Sounds\\collision_brick.wav"
  , pathSoundRacket = "Sounds\\collision_racket.wav"
  }

-- Data Types
data Racket = Racket
  { racketSpeedX :: Float
  , racketWidth :: Float
  , racketHeight :: Float
  , racketX :: Float
  , racketY :: Float
  } deriving Show

data Brick = Brick
  { brickIsNotBroken :: Bool
  , brickWidth :: Float
  , brickHeight :: Float
  , brickX :: Float
  , brickY :: Float
  } deriving Show

data Lives = Lives
  { livesCount :: Int
  , livesImg :: Picture
  , livesWidth :: Float
  , livesHeight :: Float
  } deriving Show

data Ball = Ball
  { ballWidth :: Float
  , ballHeight :: Float
  , ballSpeedX :: Float
  , ballSpeedY :: Float
  , ballX :: Float
  , ballY :: Float
  } deriving Show

  data Sound = Sound 
  {
    soundBrick :: File
  , soundRacket :: File
  }

data GameState = GameState
  { racket :: Racket
  , bricks :: [[Brick]]
  , lives :: Lives
  , ball :: Ball
  , nbBricks :: Int
  , currentPage :: Page
  , soundBrick :: Sound
  , soundRacket :: Sound
  , font :: Font
  } deriving Show

data Page = PageBeginning | PageRound | PageEnd

-- Functions racket
initializeRacket :: Racket
initializeRacket = Racket
  { racketSpeedX = 215
  , racketWidth = winWidth constants / 4
  , racketHeight = winHeight constants / 37
  , racketX = (winWidth constants - racketWidth initializeRacket) / 2
  , racketY = winHeight constants - 64
  }

close :: GameState 
close = liftIO $ do
  exitSuccess

-- Load the sound 
loadSound :: FilePath -> IO Sound
loadSound filePath = do
    --  functions 
    sound <- loadSoundFromFile filePath
    return sound

-- Define a data type to represent your loaded font
data LoadedFont = LoadedFont TTF.Font

-- Load the font and assign it to the font variable
loadFont :: FilePath -> Int -> IO LoadedFont
loadFont fontFile fontSize = do
    initializeTTF
    font <- TTF.openFont fontFile fontSize
    return (LoadedFont font)

resetRacket :: Racket -> Racket
resetRacket racket = racket { racketX = (winWidth constants - racketWidth racket) / 2, racketY = winHeight constants - 64 }

-- Functions bricks
createBrick :: Int -> Int -> Brick
createBrick line column = Brick
  { brickIsNotBroken = True
  , brickWidth = winWidth constants / fromIntegral (bricksPerLine constants) - 5
  , brickHeight = winHeight constants / 35
  , brickX = 2.5 + fromIntegral (column - 1) * (5 + brickWidth (createBrick line column))
  , brickY = fromIntegral line * (winHeight constants / 35 + 2.5)
  }

initializeBricks :: [[Brick]]
initializeBricks = [[createBrick line column | column <- [1..bricksPerLine constants]] | line <- [1..bricksPerColumn constants]]

-- Functions lives
initializeLives :: Lives
initializeLives = Lives
  { livesCount = nbLives constants
  , livesImg = love.graphics.newImage pathLife -- Load your image here
  , livesWidth = 10 -- Set your image width
  , livesHeight = 10 -- Set your image height
  }

-- Functions ball
initializeBall :: Float -> Float -> Ball
initializeBall racketHeight racketY = Ball
  { ballWidth = racketHeight * 0.75
  , ballHeight = racketHeight * 0.75
  , ballSpeedX = -defaultSpeedBx constants
  , ballSpeedY = -defaultSpeedBy constants
  , ballX = winWidth constants / 2 - racketHeight * 0.75 / 2
  , ballY = racketY - 2 * racketHeight * 0.75 - racketHeight * 0.75 / 2
  }

resetBall :: Float -> Ball -> Ball
resetBall racketY ball = ball
  { ballSpeedY = -defaultSpeedBy constants
  , ballSpeedX = fromIntegral (randomR (-round (defaultSpeedBx constants), round (defaultSpeedBx constants)) (mkStdGen 42))
  , ballX = winWidth constants / 2 - ballWidth ball / 2
  , ballY = racketY - 2 * ballHeight ball - ballHeight ball / 2
  }

-- Functions collision
collisionBallWithRacket :: Racket -> Ball -> Ball
collisionBallWithRacket racket ball = ball
  { ballSpeedY = -ballSpeedY ball
  , ballSpeedX = if ballX ball < racketX racket + 1/8 * racketWidth racket && ballSpeedX ball >= 0
                  then if ballSpeedX ball <= defaultSpeedBx constants / 2
                       then fromIntegral (randomR (-round (0.75 * defaultSpeedBx constants), round (defaultSpeedBx constants)) (mkStdGen 42))
                       else -ballSpeedX ball
                  else if ballX ball > racketX racket + 7/8 * racketWidth racket && ballSpeedX ball <= 0
                       then if ballSpeedX ball >= -defaultSpeedBx constants / 2
                            then fromIntegral (randomR (-round (0.75 * defaultSpeedBx constants), round (defaultSpeedBx constants)) (mkStdGen 42))
                            else -ballSpeedX ball
                       else ballSpeedX ball
  }

collisionBallWithBrick :: Ball -> Brick -> (Ball, Brick)
collisionBallWithBrick ball brick = if brickIsNotBroken brick
                                    then if ballX ball < brickX brick && ballSpeedX ball > 0
                                         then (ball { ballSpeedX = -ballSpeedX ball }, brick { brickIsNotBroken = False })
                                         else if ballX ball > brickX brick + brickWidth brick && ballSpeedX ball < 0
                                              then (ball { ballSpeedX = -ballSpeedX ball }, brick { brickIsNotBroken = False })
                                              else if ballY ball < brickY brick && ballSpeedY ball > 0
                                                   then (ball { ballSpeedY = -ballSpeedY ball }, brick { brickIsNotBroken = False })
                                                   else if ballY ball > brickY brick && ballSpeedY ball < 0
                                                        then (ball { ballSpeedY = -ballSpeedY ball }, brick { brickIsNotBroken = False })
                                                        else (ball, brick)
                                    else (ball, brick)

-- Functions page partie
updateRound :: Float -> GameState -> GameState
updateRound dt gameState
  | currentPage gameState == PageRound = gameState
    { racket = moveRacket (racket gameState)
    , ball = moveBall dt (ball gameState)
    , bricks = [map (snd . collisionBallWithBrick (ball gameState)) line | line <- bricks gameState]
    , nbBricks = countRemainingBricks (bricks gameState)
    , currentPage = if livesCount (lives gameState) == 0 || nbBricks gameState == 0 then PageEnd else PageRound
    }
  | otherwise = gameState
  where
    moveRacket rckt
      | KeyLeft `elem` keysDown = if racketX rckt > 0 then rckt { racketX = racketX rckt - racketSpeedX rckt * dt } else rckt
      | KeyRight `elem` keysDown = if racketX rckt + racketWidth rckt < winWidth constants then rckt { racketX = racketX rckt + racketSpeedX rckt * dt } else rckt
      | otherwise = rckt
    moveBall deltaTime bl = bl
      { ballX = ballX bl + ballSpeedX bl * deltaTime
      , ballY = ballY bl + ballSpeedY bl * deltaTime
      , ballSpeedX = if ballX bl + ballWidth bl >= winWidth constants || ballX bl <= 0 then -ballSpeedX bl else ballSpeedX bl
      , ballSpeedY = if ballY bl + ballHeight bl >= winHeight constants || ballY bl <= 0 then -ballSpeedY bl else ballSpeedY bl
      }
    countRemainingBricks lines = sum [length [() | brick <- line, brickIsNotBroken brick] | line <- lines]
    keysDown = [KeyLeft | elem 'q' keys] ++ [KeyLeft | elem 'left' keys] ++ [KeyRight | elem 'd' keys] ++ [KeyRight | elem 'right' keys]
    keys = []

-- Functions Callback
drawRound :: GameState -> Picture
drawRound gameState = pictures $
  [ translate (racketX (racket gameState)) (racketY (racket gameState)) $ color white $ rectangleSolid (racketWidth (racket gameState)) (racketHeight (racket gameState))
  , pictures [translate (brickX brick) (brickY brick) $ color white $ rectangleSolid (brickWidth brick) (brickHeight brick) | brick <- concat (bricks gameState), brickIsNotBroken brick]
  , pictures [translate (5 + fromIntegral i * 1.20 * livesWidth (lives gameState)) (winHeight constants - livesHeight (lives gameState)) $ livesImg (lives gameState) | i <- [0..livesCount (lives gameState) - 1]]
  , translate (ballX (ball gameState)) (ballY (ball gameState)) $ color white $ rectangleSolid (ballWidth (ball gameState)) (ballHeight (ball gameState))
  ]

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char 'r') Down _ _) gameState
  | currentPage gameState /= PageBeginning = gameState
    { racket = resetRacket (racket gameState)
    , bricks = [[brick { brickIsNotBroken = True } | brick <- line] | line <- bricks gameState]
    , lives = Lives { livesCount = nbLives constants, livesImg = livesImg (lives gameState), livesWidth = livesWidth (lives gameState), livesHeight = livesHeight (lives gameState) }
    , nbBricks = bricksPerColumn constants * bricksPerLine constants
    , ball = resetBall (racketY (racket gameState)) (ball gameState)
    , currentPage = PageRound
    }
  | otherwise = gameState
handleKeys (EventKey (Char 'q') Down _ _) gameState = gameState { keysDown = 'q' : keysDown gameState }
handleKeys (EventKey (Char 'q') Up _ _) gameState = gameState { keysDown = filter (/= 'q') (keysDown gameState) }
handleKeys (EventKey (Char 'd') Down _ _) gameState = gameState { keysDown = 'd' : keysDown gameState }
handleKeys (EventKey (Char 'd') Up _ _) gameState = gameState { keysDown = filter (/= 'd') (keysDown gameState) }
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gameState = gameState { keysDown = KeyLeft : keysDown gameState }
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) gameState = gameState { keysDown = filter (/= KeyLeft) (keysDown gameState) }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState = gameState { keysDown = KeyRight : keysDown gameState }
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) gameState = gameState { keysDown = filter (/= KeyRight) (keysDown gameState) }
handleKeys (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
handleKeys _ gameState = gameState

main :: IO ()
main = do
  let initialState = GameState
        { racket = initializeRacket
        , bricks = initializeBricks
        , lives = initializeLives
        , ball = initializeBall (racketHeight (racket initializeRacket)) (racketY (racket initializeRacket))
        , nbBricks = bricksPerColumn constants * bricksPerLine constants
        , currentPage = PageBeginning
        , soundBrick = loadSound  pathSoundBrick    -- Load your sound here
        , soundRacket = loadSound  pathSoundRacket  -- Load your sound here
        , font =  loadFont "arial.ttf" 18 -- Load your font here
        }
  play
    (InWindow "Bricksbreak" (round (winWidth constants), round (winHeight constants)) (10, 10))
    black
    60
    initialState
    drawRound
    handleKeys
    (\_ gameState -> gameState)
