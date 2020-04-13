module Lib (
  Color, Board, Coordinates, Position, GameState(GameState),
  getAvailableMoves,
  getCellsToUpdate,
  updateBoard,
  gameStateToString,
  legalMovesToString,
  invertColor,
  initialGameState,
  calculateScore
)  where

import Data.List
import qualified Data.Vector as V
import qualified Data.List.Split as S


-- Exported functions:

-- getAvailableMoves  :: Board -> Color -> [Coordinates]
-- getCellsToUpdate   :: Board -> Color -> Coordinates -> [Coordinates]
-- updateBoard        :: Color -> [Coordinates] -> Board -> Board
-- gameStateToString  :: GameState -> String
-- legalMovesToString :: [Coordinates] -> String
-- invertColor        :: Color -> Color
-- calculateScore     :: Board -> String


-- ********************************************************
-- **********    BASIC DATATYPES + UTILITIES   ************
-- ********************************************************

-- the dimensions of board
boardWidth  = 8
boardHeight = 8

-- each cell on the board can either be a Blank cell or 
-- contain a piece of specific color (Black or White) 
data Cell = Blank | FilledCell Color deriving (Show, Eq)
data Color = Black | White deriving (Show, Eq)

-- board represented as one-dimensional Vector for efficiency
type Board = V.Vector Cell

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
initialBoard = boardWithPieces
  where
    -- put generated pieces to the empty board
    boardWithPieces = replaceCellsInBoard initialFilledCells blankBoard
    -- add multiple colored pieces on the board
    initialFilledCells = [((y, x), (FilledCell color)) | 
                            y     <- [3,4], 
                            x     <- [3,4], 
                            color <- [if x == y then White else Black]]

    -- produce empty board with blank cells
    blankBoard         = V.replicate (boardHeight * boardWidth) Blank

-- convert the board to pretty-printable String
boardToString :: Board -> String
boardToString board = formatLines boardList
  where formatLines = concatMap $ (++ "\n") . (intersperse ',') . (map cellToChar)
        boardList   = S.chunksOf boardWidth (V.toList board)

-- test if given coordinates are located outside the bounds of board (1..8, 1..8)
isOutOfBoard :: Coordinates -> Bool
isOutOfBoard (y, x) = y < 0 || y >= boardHeight || x < 0 || x >= boardWidth

-- convert 2-D coordinates to vector index
-- assumes that coordinates are within bounds of the board
coordsToIndex :: Coordinates -> Int
coordsToIndex (y, x) = y * boardWidth + x

-- Convert GameState to string to the string in the following format:
-- CurrentPlayer: <Color>
-- <BOARD>
gameStateToString :: GameState -> String
gameStateToString (GameState board color) = concat $ 
  intersperse "\n" ["Current player: " ++ (show color), boardToString board]

-- calculate score based on current board state and return print message
calculateScore :: Board -> String
calculateScore board = whiteScore ++ "\n" ++ blackScore
  where whiteScore = "White Player score: " ++ scoreToString White
        blackScore = "Black Player score: " ++ scoreToString Black
        scoreToString color = show . length $ V.filter (== FilledCell color) board

-- Convert the list of coordinates to string in the following format:
-- 1. (a,b)
-- 2. (c,d)
-- 3. etc..
legalMovesToString :: [Coordinates] -> String
legalMovesToString coordinates = concatMap tupleToString $ labeledCoordinates
  where
    labeledCoordinates = zip [1..] coordinates
    tupleToString (index, coords) = show index ++ ". " ++ show coords ++ "\n"

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
getCellFromBoard board coords = board V.! (coordsToIndex coords)

-- replace single element in list by new element
replaceItemInList :: Position -> a -> [a] -> [a]
replaceItemInList position newElement l = 
  let (left, (_:right)) = splitAt position l
  in left ++ [newElement] ++ right

-- given the input board, replace the elements with specific coordinates and return the updated board
replaceCellsInBoard :: [(Coordinates, Cell)] -> Board -> Board
replaceCellsInBoard xs board = board V.// map toIndex xs
  where toIndex (coords, cell) = (coordsToIndex coords, cell)

-- ***************************************
-- **********    GAME LOGIC   ************
-- ***************************************

-- put pieces with new color on given coordinates and return updated board
updateBoard :: Color -> [Coordinates] -> Board -> Board
updateBoard color coords board = replaceCellsInBoard (zip coords colorList) board
  where colorList = repeat (FilledCell color)

-- given current board state and coordinates (where we would like to put new piece (black or white)),
-- return the list of coordinates, where we can put new color
getCellsToUpdate :: Board -> Color -> Coordinates -> [Coordinates]
getCellsToUpdate board color coordinates = cellsToUpdate
  where
    -- all coordinates that have to be updated + initial coordinates 
    -- (where player would like to put his new piece)
    cellsToUpdate = extractedCoordinates ++ [coordinates]
    -- extract all coordinates, that have to be updated on current turn
    extractedCoordinates = map fst . concat . map keepOnlyInverseCells $ filteredPaths
    -- keed only paths that are legal
    filteredPaths = filter (isLegalPath color . map snd) paths
    -- generate paths in all possible directions
    paths = [generatePathOnBoard board coordinates direction | direction <- allPossibleDirections]
    -- given list of cells, keep only those cells, that conforms to the color, 
    -- that is opposite to the current player's color
    keepOnlyInverseCells = (takeWhile ((== invertedCell) . snd) . tail) 
    -- cell with inverted color
    invertedCell = FilledCell $ invertColor color

-- scan the entire board and return the list of available moves for the given player color
getAvailableMoves :: Board -> Color -> [Coordinates]
getAvailableMoves board color = [(y, x) | y <- [0..boardHeight], 
                                          x <- [0..boardWidth], 
                                          isMoveLegal board color (y,x)]

-- check if player can perform a move (i.e put new piece with his color) on a cell with given coordinates
-- at least one path should be available for the player, 
-- otherwhise player cannot make the turn on given cell
isMoveLegal :: Board -> Color -> Coordinates -> Bool
isMoveLegal board color coordinates = legalPathExists
  where
    -- check if at least one legal path exists
    legalPathExists = any (isLegalPath color) paths
    -- generate all possible paths from given cell (in all possible directions)
    paths = [map snd $ generatePath direction | direction <- allPossibleDirections] 
    -- generate path with given direction      
    generatePath = generatePathOnBoard board coordinates

-- given the current player's color, check if path is legal by checking all the color along the path
isLegalPath :: Color -> [Cell] -> Bool
isLegalPath currentColor (Blank:xs) = inverseColorExistsOnPath && restPathIsValid
  where 
    -- the should be at least >1 cells with the opposite color along the path
    inverseColorExistsOnPath = not . null $ invertColorPath
    -- extract path, where cells have the opposite color
    invertColorPath = takeWhile invertColorExistsInCell xs
    -- extract the rest of the path by removing cell with opposite color from the beggining of the path
    restPath = dropWhile invertColorExistsInCell xs 
    -- check if the rest oath is valid (it should not be empty and the first cell should have the correct color)
    restPathIsValid = ((not . null) restPath) && (head restPath == (FilledCell currentColor))
    -- check if current cell is not blank and have the opposite color
    invertColorExistsInCell (FilledCell c) = invertColor currentColor == c
    invertColorExistsInCell Blank          = False

-- first cell on the path is not Blank, so the player cannot put his piece on this path
isLegalPath _ _ = False

-- by giving the initial coordinates and direction, generate path along this direction
generatePathOnBoard :: Board -> Coordinates -> Direction -> [(Coordinates, Cell)]
generatePathOnBoard board coordinates direction = generatedPath
  where        
    -- generate path and label each cell along the path with its Coordinates
    generatedPath = map labelCellWithCoordinates $ generatePath coordinates direction
    -- given the coordinates, extract cell from the board and return the tuple (Coordinates, Cell)
    labelCellWithCoordinates coordinates = (coordinates, getCellFromBoard board coordinates)

-- given starting position and direction, generate the list of coordinates, 
-- that will be visited along with direction until the boundaries of board are reached
generatePath :: Coordinates -> Direction -> [Coordinates]
generatePath coordinates direction = filteredPath
  where
    filteredPath = takeWhile isInBoard path
    path = iterate (moveInDirection direction) coordinates
