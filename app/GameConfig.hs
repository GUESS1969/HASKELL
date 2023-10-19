-- GameConfig.hs
-- Module containing game-related configurations and constants.

module GameConfig
    ( screenWidth
    , screenHeight
    , ballChar
    , brickChar
    , brickWidth
    , brickHeight
    , racketWidth
    , racketHeight
    ) where

-- Screen dimensions
screenWidth :: Int
screenWidth = 45

screenHeight :: Int
screenHeight = 24

-- Ball representation
ballChar :: Char
ballChar = 'O'

-- Brick dimensions and representation
brickWidth :: Int
brickWidth = 8
brickHeight :: Int
brickHeight = 2
brickChar :: Char
brickChar = '#'

-- Racket dimensions
racketWidth :: Int
racketWidth = 5
racketHeight :: Int
racketHeight = 1