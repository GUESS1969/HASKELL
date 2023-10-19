-- | GameTypes.hs
-- Module for defining the game's data structures and related functions.
module GameTypes
    ( Rect(..)
    , Ball(..)
    , Racket(..)
    , Brick(..)
    , Game(..)
    , GameStatus(..)
    , initializeRect
    , initialGame
    ) where

-- | Rect data type represents a grid-based rectangle shape.
data Rect = Rect
    { rectX      :: Int  -- ^ X coordinate of the rectangle.
    , rectY      :: Int  -- ^ Y coordinate of the rectangle.
    , rectWidth  :: Int  -- ^ Width of the rectangle.
    , rectHeight :: Int  -- ^ Height of the rectangle.
    } deriving (Show, Eq)

-- | Ball data type represents the game ball.
data Ball = Ball
    { ballRect  :: Rect      -- ^ Grid data of the ball, using the 'Rect' type.
    , ballVelX  :: Int       -- ^ Horizontal velocity of the ball.
    , ballVelY  :: Int       -- ^ Vertical velocity of the ball.
    , ballChar  :: Char      -- ^ Textual representation of the ball.
    } deriving (Show, Eq)

-- | Racket data type represents the player's racket.
data Racket = Racket
    { racketRect :: Rect     -- ^ Grid data of the racket, using the 'Rect' type.
    , racketStr  :: String   -- ^ Textual representation of the racket.
    } deriving (Show, Eq)

-- | Brick data type represents a brick in the game.
data Brick = Brick
    { brickRect :: Rect     -- ^ Grid data of the brick, using the 'Rect' type.
    , brickStr  :: String   -- ^ Textual representation of the brick.
    } deriving (Show, Eq)

-- | Enum for representing the current status of the game.
data GameStatus
    = Running               -- ^ The game is ongoing.
    | Paused                -- ^ The game is paused.
    | GameOver              -- ^ The game has ended with a loss.
    | Victory               -- ^ The game has ended with a win.
    deriving (Show, Eq)

-- | Game data type represents the overall game state.
data Game = Game
    { gameBall      :: Ball          -- ^ The game's ball.
    , gameRacket    :: Racket        -- ^ The player's racket.
    , gameBricks    :: [Brick]       -- ^ A list of all bricks in the game.
    , gameStatus    :: GameStatus    -- ^ The current status of the game.
    , gameScore     :: Int           -- ^ The current score.
    } deriving (Show, Eq)

-- | Function to initialize a 'Rect' with provided parameters.
initializeRect :: Int -> Int -> Int -> Int -> Rect
initializeRect = Rect

initialGame :: Game
initialGame = Game
    { gameBall = Ball 
        { ballRect = initializeRect 5 5 1 1
        , ballVelX = 1
        , ballVelY = 1
        , ballChar = 'O'
        }
    , gameRacket = Racket 
        { racketRect = initializeRect 0 20 10 1
        , racketStr = "=========="
        }
    , gameBricks = [Brick { brickRect = initializeRect x 1 4 1, brickStr = "====" } | x <- [0,5..40]]
    , gameStatus = Running
    , gameScore = 0
    }
