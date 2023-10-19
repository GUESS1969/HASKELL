module Main where

import GameTypes( Game(..), initialGame, GameStatus( GameOver ) )
import GameInput ( updateGame, handleInput )
import GameRenderCLI ( printGameState, )
import System.Console.ANSI (clearScreen )
import System.IO ( hSetEcho, stdin, hSetBuffering, BufferMode(..), hReady, hGetChar )
import Control.Concurrent ( threadDelay )

-- This is your main game function
runGame :: IO ()
runGame = do
    hSetEcho stdin False       -- Turn off terminal echoing
    hSetBuffering stdin NoBuffering -- Make stdin unbuffered
    gameLoop initialGame       -- Start the game loop with the initial game state

main :: IO ()
main = runGame

gameLoop :: Game -> IO ()
gameLoop game = 
    case gameStatus game of
        GameOver -> displayGameOver
        _ -> do
            userInput <- getInput
            let updatedGame = handleInput userInput game
            let newGame = updateGame updatedGame
            --clearScreen                -- Clear the console
            -- OR
            resetCursor                -- Move cursor to the top of the screen
            displayScore newGame       -- Display the score first
            putStr (printGameState newGame)     -- Render the game state
            threadDelay 500000 -- Delay for 0.1 seconds (100000 microseconds)   
            gameLoop newGame  -- Continue the loop with the new game state

getInput :: IO Char
getInput = do
    hasInput <- hReady stdin
    if hasInput then getChar else return ' ' -- Return ' ' if no input

displayScore :: Game -> IO ()
displayScore game = putStrLn $ "\ESC[32mScore: " ++ show (gameScore game) ++ "\ESC[0m"

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J" 

-- Déplace le curseur vers le haut de l'écran
resetCursor :: IO ()
resetCursor = putStr "\ESC[H"

displayGameOver :: IO ()
displayGameOver = putStrLn "\ESC[1;31;5mGame Over!\ESC[0m"
