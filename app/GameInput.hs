-- Module for handling game , Input , Data an


module GameInput
    ( updateGame
    , handleInput
    ) where
import GameTypes ( Game(..), Ball(..), Rect(..), Racket(..), Brick(..), GameStatus(..) )
import Data.List (elem)
import GameConfig ( screenWidth, screenHeight, ballChar, brickChar, brickWidth, brickHeight, racketWidth, racketHeight)


-- Constants
ballSpeed :: Int
ballSpeed = 5

racketSpeed :: Int
racketSpeed = 5

-- Handle player input to move the racket
handleInput :: Char -> Game -> Game
handleInput 'a' game = if rectX (racketRect (gameRacket game)) > 0 
                       then moveRacket (-racketSpeed) game
                       else game
handleInput 'd' game = if rectX (racketRect (gameRacket game)) + rectWidth (racketRect (gameRacket game)) < screenWidth
                      then moveRacket racketSpeed game
                       else game
handleInput _ game = game

-- Moving the Racket function
moveRacket :: Int -> Game -> Game
moveRacket dx game = 
    let racket = gameRacket game
        racket' = racket { racketRect = (racketRect racket) { rectX = rectX (racketRect racket) + dx }}
    in game { gameRacket = racket' }

-- Update the game based on logic
updateGame :: Game -> Game
updateGame game 
    | gameStatus game == GameOver || gameStatus game == Victory = game
    | ballOutOfBound (gameBall game) = game { gameStatus = GameOver }  -- Si le ballon est hors de l'écran
    | null (gameBricks game) = game { gameStatus = GameOver }  -- Si tous les briques sont éliminés
    | otherwise = 
        let game' = moveBall game
            game'' = detectCollisions game'
        in game''

-- Function whether Ball is out of the field.
ballOutOfBound :: Ball -> Bool
ballOutOfBound ball = 
    rectY (ballRect ball) + rectHeight (ballRect ball) >= screenHeight  -- Supposons que screenHeight est la hauteur de l'écran

-- Move the ball based on its velocity
moveBall :: Game -> Game
moveBall game = 
    let ball = gameBall game
        ball' = ball { ballRect = newBallRect }
        newBallRect = 
            let currentRect = ballRect ball
                newRectX = rectX currentRect + ballVelX ball
                newRectY = rectY currentRect + ballVelY ball
            in currentRect { rectX = if newRectX < 0 || newRectX + rectWidth currentRect > screenWidth
                                     then rectX currentRect - ballVelX ball
                                     else newRectX
                          , rectY = if newRectY < 0 || newRectY + rectHeight currentRect > screenHeight
                                     then rectY currentRect - ballVelY ball
                                     else newRectY }
    in game { gameBall = ball' }

-- Checks if the ball's new position is within screen boundaries and adjusts if necessary
checkBoundaries :: Rect -> Int -> Int -> Rect
checkBoundaries rect dx dy 
    | newX < 0               = rect { rectX = 0, rectY = newY }
    | newX + rectWidth rect > screenWidth = rect { rectX = screenWidth - rectWidth rect, rectY = newY }
    | newY < 0               = rect { rectX = newX, rectY = 0 }
    | newY + rectHeight rect > screenHeight = rect { rectX = newX, rectY = screenHeight - rectHeight rect }
    | otherwise              = rect { rectX = newX, rectY = newY }
  where
    newX = rectX rect + dx
    newY = rectY rect + dy

-- Detect and resolve collisions
detectCollisions :: Game -> Game
detectCollisions game =
    let ball = gameBall game
        racket = gameRacket game
        bricks = gameBricks game
        gameWithRacketCollision = if checkCollision ball (Left racket) then handleRacketCollision game ball else game
        gameWithBrickCollision = foldl handleBrickCollision gameWithRacketCollision bricks
    in handleBoundaryCollision gameWithBrickCollision
-- To handle the collision 
handleBoundaryCollision :: Game -> Game
handleBoundaryCollision game =
    let ball = gameBall game
        ballRect' = ballRect ball
        ball' = if rectX ballRect' <= 0 || rectX ballRect' + rectWidth ballRect' >= screenWidth then ball { ballVelX = -ballVelX ball } else ball
        ball'' = if rectY ballRect' <= 0 || rectY ballRect' + rectHeight ballRect' >= screenHeight then ball' { ballVelY = -ballVelY ball } else ball'
    in game { gameBall = ball'' }

-- Check collision between ball and a rectangle object (racket or brick)
checkCollision :: Ball -> Either Racket Brick -> Bool
checkCollision ball object = 
    let ballR = ballRect ball
        objectR = case object of
            Left racket -> racketRect racket
            Right brick -> brickRect brick
    in rectsCollide ballR objectR

rectsCollide :: Rect -> Rect -> Bool
rectsCollide a b = 
    rectX a < rectX b + rectWidth b &&
    rectX a + rectWidth a > rectX b &&
    rectY a < rectY b + rectHeight b &&
    rectY a + rectHeight a > rectY b

handleRacketCollision :: Game -> Ball -> Game
handleRacketCollision game ball
    | isCollision = game { gameBall = newBall }
    | otherwise   = game
  where
    racket = gameRacket game
    racketRect' = racketRect racket
    ballRect' = ballRect ball
    isCollision = rectsCollide ballRect' racketRect'  -- Use rectsCollide for the check here
    newBall = ball { ballVelY = -ballVelY ball }

handleBrickCollision :: Game -> Brick -> Game
handleBrickCollision game brick
    | isCollision = game { gameBricks = newBricks, gameScore = gameScore game + 1 }
    | otherwise   = game
  where
    ball = gameBall game
    ballRect' = ballRect ball
    brickRect' = brickRect brick
    bricks = gameBricks game
    isCollision = rectsCollide ballRect' brickRect'  -- Use rectsCollide for the check here
    newBricks = filter (/= brick) bricks