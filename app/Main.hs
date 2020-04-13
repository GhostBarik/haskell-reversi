import Data.List
import Text.Read

-- the dimensions of board
boardWidth  = 8
boardHeight = 8

-- each cell on the board ca either be a Blank cell or 
-- contain a piece of specific color (Black or White) 
data Cell = Blank | FilledCell Color deriving (Show, Eq)
data Color = Black | White deriving (Show, Eq)

-- board represented as two-dimensional list
type Board = [[Cell]]

-- type aliases for common data types for better readability
type Position = Int
type Coordinates = (Position, Position)

data Direction = DLeft    | DRight    | DTop        | DBottom | 
                 DTopLeft | DTopRight | DBottomLeft | DBottomRight

-- game state is represented as board + current player color
data GameState = GameState Board Color

-- flip the given color to produce the opposite color
-- (we need this to flip back and forth between 2 players during the game)
invertColor :: Color -> Color
invertColor White = Black
invertColor Black = White

-- initial state is represented as board with 4 pieces (2 black + 2 white) and 
-- Black player who take the first turn in the game
initialGameState = GameState initialBoard Black

-- list of all possible directions
allPossibleDirections :: [Direction]
allPossibleDirections = [DLeft, DRight, DTop , DBottom, DTopLeft, 
                         DTopRight, DBottomLeft, DBottomRight]

-- given specific direction and coordinates in the board, 
-- make a single step in the following direction and produce new coordinates
moveInDirection :: Direction -> Coordinates -> Coordinates
moveInDirection DLeft        (y, x) = (y, x - 1)
moveInDirection DRight       (y, x) = (y, x + 1)
moveInDirection DTop         (y, x) = (y - 1, x)
moveInDirection DBottom      (y, x) = (y + 1, x)
moveInDirection DTopLeft     (y, x) = (y - 1, x - 1)
moveInDirection DTopRight    (y, x) = (y - 1, x + 1)
moveInDirection DBottomLeft  (y, x) = (y + 1, x - 1)
moveInDirection DBottomRight (y, x) = (y + 1, x + 1)

-- create initial board with size 8x8 and put 4 pieces onto it (2 black + 2 white)
initialBoard :: Board
initialBoard = foldl (.) id fillFunctions blankBoard 

    -- add multiple colored pieces on the board
    where fillFunctions = [replaceCellInBoard (y, x) (FilledCell color) | 
                          y     <- [3,4], 
                          x     <- [3,4], 
                          color <- [if x == y then White else Black]]

          -- produce empty board with blank cells
          blankBoard    = replicate boardHeight row
          row           = replicate boardWidth Blank

-- convert the board to pretty-printable String
boardToString :: Board -> String
boardToString = concatMap $ (++ "\n") . (intersperse ',') . (map cellToChar)

-- test if given coordinates are located outside the bounds of board (1..8, 1..8)
isOutOfBoard :: Coordinates -> Bool
isOutOfBoard (y, x) = y < 0 || y >= boardHeight || x < 0 || x >= boardWidth

-- test if given coordinates are located within the bounds of board (1..8, 1..8)
isInBoard :: Coordinates -> Bool
isInBoard = not . isOutOfBoard

-- convert the cell to pretty-printable Character
cellToChar :: Cell -> Char
cellToChar Blank = '_'
cellToChar (FilledCell Black) = 'x'
cellToChar (FilledCell White) = 'o'

-- extract the specific cell (with specific {X,Y} coordinates) from board
getCellFromBoard :: Board -> Coordinates -> Cell
getCellFromBoard board (y, x) = board !! y !! x

-- replace single element in list by new element
replaceItemInList :: Position -> a -> [a] -> [a]
replaceItemInList position newElement l = 
    let (left, (_:right)) = splitAt position l
    in left ++ [newElement] ++ right

-- given the input board, replace the element with specific coordinates and return the updated board
replaceCellInBoard :: Coordinates -> Cell -> Board -> Board
replaceCellInBoard (y,x) c b = replaceItemInList y newRow b
    where newRow = replaceItemInList x c (b !! y)

--------------------------------------------------------------------------------------------------

-- scan the entire board and return the list of available moves for the given player color
getAvailableMoves :: Board -> Color -> [Coordinates]
getAvailableMoves board color = [(y, x) | y <- [0..boardHeight], 
                                          x <- [0..boardWidth], 
                                          isMoveLegal board color (y,x)]

-- check if player can perform a move (i.e put new piece with his color) on a cell with given coordinates
-- at least one path should be available for the player, 
-- otherwhise player cannot make the turn on given cell
isMoveLegal :: Board -> Color -> Coordinates -> Bool
isMoveLegal board color coordinates = any (isLegalPath color) paths
    -- generate all possible paths from given cell (in all possible directions)
    where paths = [map snd $ generatePath direction | direction <- allPossibleDirections] 
          -- generate path with given direction      
          generatePath = generatePathOnBoard board coordinates

-- given current board state and coordinates (where we would like to put new piece (black or white)),
-- return the list of coordinates, where we can put new color
getCellsToUpdate :: Board -> Color -> Coordinates -> [Coordinates]
getCellsToUpdate board color coordinates = extractedCoordinates ++ [coordinates]
    where 
          -- extract all coordinates, that have to be updated on current turn
          extractedCoordinates = map fst . concat . map keepOnlyInverseCells $ filteredPaths
          -- keed only paths that are legal
          filteredPaths = filter (isLegalPath color . map snd) paths
          -- generate paths in all possible directions
          paths = [generatePathOnBoard board coordinates direction | direction <- allPossibleDirections]
          -- given list of cells, keep only those cells, that conforms to the color, 
          -- that is opposite to the current player's color
          keepOnlyInverseCells = (takeWhile ((== inverseCell) . snd) . tail) 
          -- invert color in the
          inverseCell = FilledCell $ invertColor color
        
-- put pieces with new color on given coordinates and return updated board
updateBoard :: Color -> [Coordinates] -> Board -> Board
updateBoard color coords board = foldl replaceCell board coords
    -- put single piece on the board on given position
    where replaceCell board coordinates = replaceCellInBoard coordinates (FilledCell color) board

-- given the current player's color, check if path is legal by checking all the color along the path
isLegalPath :: Color -> [Cell] -> Bool
isLegalPath currentColor (Blank:xs) = inverseColorExistsOnPath && restPathIsValid
    -- the should be at least >1 cells with the opposite color along the path
    where inverseColorExistsOnPath = not . null $ invertColorPath
          -- extract path, where cells have the opposite color
          invertColorPath = takeWhile invertColorExistsInCell xs
          -- extract the rest of the path by removing cell with opposite color from the beggining of the path
          restPath = dropWhile invertColorExistsInCell xs 
          -- check if the rest oath is valid (it should not be empty and the first cell should have the correct color)
          restPathIsValid = ((not . null) restPath) && (head restPath == (FilledCell currentColor))
          -- check if current cell is not blank and have the opposite color
          invertColorExistsInCell (FilledCell c) = invertColor currentColor == c
          invertColorExistsInCell Blank = False

-- first cell on the path is not Blank, so the player cannot put his piece on this path
isLegalPath _ _ = False

-- by giving the initial coordinates and direction, generate path along this direction
generatePathOnBoard :: Board -> Coordinates -> Direction -> [(Coordinates, Cell)]
generatePathOnBoard board coordinates direction = generatedPath
    -- generate path and label each cell along the path with its coordinates
    where generatedPath = map labelCellWithCoordinates $ generatePath coordinates direction
          -- given the coordinates, extract cell from the board and return the tuple (Coordinates, Cell)
          labelCellWithCoordinates coordinates = (coordinates, getCellFromBoard board coordinates)

-- given starting position and direction, generate the list of coordinates, 
-- that will be visited along with direction until the boundaries of board are reached
generatePath :: Coordinates -> Direction -> [Coordinates]
generatePath coordinates direction = takeWhile isInBoard path
    where path = iterate (moveInDirection direction) coordinates

-- main application entry point, that runs the game loop, 
-- starting with initial game state
main :: IO ()
main = gameLoop initialGameState

-- Convert GameState to string to the string in the following format:
-- CurrentPlayer: <Color>
-- <BOARD>
gameStateToString :: GameState -> String
gameStateToString (GameState board color) = concat $ 
  intersperse "\n" ["Current player: " ++ (show color), boardToString board]

-- Convert the list of coordinates to string in the following format:
-- 1. (a,b)
-- 2. (c,d)
-- 3. etc..
legalMovesToString :: [Coordinates] -> String
legalMovesToString coordinates = concatMap tupleToString $ zip [1..] coordinates
    where tupleToString (index, coords) = show index ++ ". " ++ show coords ++ "\n"

-- main game loop that runs all the computations and 
-- asks the players to perform their turns 
-- through the interaction via command-line (using IO monad)
gameLoop :: GameState -> IO ()
gameLoop gameState @ (GameState board color) = do
    -- display the current state of the game to the player
    putStr $ gameStateToString gameState
    -- calculate the available moves for the current player
    let availableMoves = getAvailableMoves board color
    -- calculate the available moves for the opposite player
    let oppositePlayerAvailableMoves = getAvailableMoves board (invertColor color)
    -- if both player have no available moves left
    if null availableMoves && null oppositePlayerAvailableMoves then do
        -- display scores for both players
        putStrLn $ "TODO: calculate points and print score!"
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


