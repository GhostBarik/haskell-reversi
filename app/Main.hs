module Main where

import Lib
import Text.Read

-- **************************************
-- **********    MAIN LOOP   ************
-- **************************************

-- main application entry point, that runs the game loop, 
-- starting with initial game state
main :: IO ()
main = gameLoop initialGameState

-- main game loop that runs all the computations and 
-- asks the players to perform their turns 
-- through the interaction via command-line (using IO monad)
gameLoop :: GameState -> IO ()
gameLoop gameState @ (GameState board color) = do
    -- display the current score
    putStrLn $ calculateScore board
    -- display the current state of the game to the player
    putStr $ gameStateToString gameState
    -- calculate the available moves for the current player
    let availableMoves = getAvailableMoves board color
    -- calculate the available moves for the opposite player
    let oppositePlayerAvailableMoves = getAvailableMoves board (invertColor color)
    -- if both player have no available moves left
    if null availableMoves && null oppositePlayerAvailableMoves then do
        -- display the current score
        putStrLn $ calculateScore board
        -- finish the game
        putStrLn $ "The Game is finished! Thank you for playing!"
    -- if current player do not have empty places where he can put new piece
    else if null availableMoves then
        -- switch to the other player and continue the game
        gameLoop (GameState board (invertColor color))
    else
        -- ask player to make the turn using available list of available positions on the board
        makeTurn gameState availableMoves

-- given the current state of the game and list of available player moves, 
-- ask the player to make the turn 
makeTurn :: GameState -> [Coordinates] -> IO ()
makeTurn gameState @ (GameState board color) legalMoves = do
    -- ask player to pick one the available positions
    putStrLn "Choose your move: "
    -- display available options to the player
    putStr $ legalMovesToString legalMoves
    -- calculate the number of available options (used in boundary check later)
    let maxIndex = length legalMoves
    -- error message, that is displayed when player choses an invalid index
    let invalidIndexMessage = "Invalid index! Please choose another index."
    -- read the player choice from the command line
    line <- getLine
    -- test the selected option
    case (readMaybe line :: Maybe Int) of 
        -- player picked an incorrect option
        Just index | index > maxIndex || index < 1 -> do
            -- display error message 
            putStrLn $ invalidIndexMessage
            -- restart the turn
            makeTurn gameState legalMoves
        -- player picked the correct option (index)
        Just index -> do
            -- extract selected option (coordinates) from the list
            let chosenCoordinates = legalMoves !! (index-1)
            -- display picked option
            putStrLn $ "You picked the following coordinate: " ++ show chosenCoordinates
            -- calculate the list of cells, that have to be updated
            let cellsToUpdate = getCellsToUpdate board color chosenCoordinates
            -- update the board
            let newBoard = updateBoard color cellsToUpdate board
            -- switch to the other player and continue the game
            gameLoop (GameState newBoard (invertColor color))
        Nothing -> do
            -- display error message
            putStrLn $ invalidIndexMessage
            -- restart the turn
            makeTurn gameState legalMoves


