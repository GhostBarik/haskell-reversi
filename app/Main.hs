import Data.List

-- the dimensions of board
boardWidth  = 8
boardHeight = 8

-- cell on the board 
data Cell = Blank | FilledCell Color deriving (Show, Eq)
data Color = Black | White deriving (Show, Eq)

-- board represented as two-dimensional list
type Board = [[Cell]]

-- type aliases for common data types for better readability
type Position = Int
type Coordinates = (Position, Position)

data Direction = DLeft | DRight | DTop | DBottom | DTopLeft | DTopRight | DBottomLeft | DBottomRight

moveInDirection :: Direction -> Coordinates -> Coordinates
moveInDirection DLeft        (y, x) = (y, x - 1)
moveInDirection DRight       (y, x) = (y, x + 1)
moveInDirection DTop         (y, x) = (y - 1, x)
moveInDirection DBottom      (y, x) = (y + 1, x)
moveInDirection DTopLeft     (y, x) = (y - 1, x - 1)
moveInDirection DTopRight    (y, x) = (y - 1, x + 1)
moveInDirection DBottomLeft  (y, x) = (y + 1, x - 1)
moveInDirection DBottomRight (y, x) = (y + 1, x + 1)

invertColor :: Color -> Color
invertColor White = Black
invertColor Black = White

-- initial board state for our game
-- create initial board with size 8x8
intialBoard :: Board
intialBoard = foldl (.) id fillFunctions blankBoard 
    where fillFunctions = [replaceCellInBoard (y, x) color | 
                          y <- [3,4], x <- [3,4], 
                          color <- if x == y then [FilledCell White] else [FilledCell Black]]
          blankBoard    = replicate boardHeight row
          row           = replicate boardWidth Blank

-- convert the board to pretty-printable String
boardToString :: Board -> String
boardToString = concatMap $ (++ "\n") . (intersperse ',') . (map cellToChar)

isOutOfBoard :: Coordinates -> Bool
isOutOfBoard (y, x) = y < 0 || y >= boardHeight || x < 0 || x >= boardWidth

isInBoard :: Coordinates -> Bool
isInBoard = not . isOutOfBoard

-- converts the cell to pretty-printable Character
cellToChar :: Cell -> Char
cellToChar Blank = '_'
cellToChar (FilledCell Black) = 'x'
cellToChar (FilledCell White) = 'o'

-- get the specific cell (with specific {X,Y} coordinates) from board 
getCellFromBoard :: Board -> Coordinates -> Cell
getCellFromBoard board (y, x) = board !! y !! x

-- replace element in list by new element (in specific position)
replaceItemInList :: Position -> a -> [a] -> [a]
replaceItemInList position newElement l = 
    let (left, (_:right)) = splitAt position l -- split list and remove element on given position
    in left ++ [newElement] ++ right           -- put new element and merge list back

-- replaces an element with specific coordinates on given board
replaceCellInBoard :: Coordinates -> Cell -> Board -> Board
replaceCellInBoard (y,x) c b = replaceItemInList y newRow b
    where newRow = replaceItemInList x c (b !! y)

--------------------------------------------------------------------------------------------------

getLegalMoves :: Board -> Color -> [Coordinates]
getLegalMoves board color = filter (isMoveLegal board color) [(y, x) | y <- [0..boardHeight], x <- [0..boardWidth]]

-- function to check if a move on a given Position with a given FilledCell is legal
isMoveLegal :: Board -> Color -> Coordinates -> Bool
isMoveLegal board color coordinates = 
    any (isLegalPath color) paths
         where paths = [map snd $ generatePathOnBoard board coordinates direction | 
                        direction <- [DLeft, DRight, DTop , DBottom, DTopLeft, DTopRight, DBottomLeft, DBottomRight]]

-- given current board state and coordinates (where we would like to put new piece (black or white)),
-- return the list of coordinates, where we can put new color

-- PRECONDITIONS: Coordinates should be valid (we can safely put our piece in this coordinates)
-- PRECONDITIONS: Input coordinates should be be empty at the beginning
-- POSTCONDITIONS: coordinates of new piece should also be included in the list of output [Coordinates]

-- TODO: use isMoveLegal function as an inspiration, but return [Coordinates] instead of Boolean
getCellsToUpdate :: Board -> Color -> Coordinates -> [Coordinates]
getCellsToUpdate board color coordinates = (extractCoordinates filtered) ++ [coordinates]
    where 
          extractCoordinates :: [[(Coordinates, Cell)]] -> [Coordinates]
          extractCoordinates = map fst . concat . map keepOnlyInverseCells

          filtered = filter (isLegalPath color . map snd) paths
          paths = [generatePathOnBoard board coordinates direction | 
                   direction <- [DLeft, DRight, DTop, DBottom, DTopLeft, DTopRight, DBottomLeft, DBottomRight]]
          
          keepOnlyInverseCells :: [(Coordinates, Cell)] -> [(Coordinates, Cell)]
          keepOnlyInverseCells = (takeWhile ((== inverseCell) . snd) . tail) 
          
          inverseCell = FilledCell $ invertColor color
        

-- mark all cells on given coordinates with new color 
-- PRECONDITIONS: list of [Coordinates] should be valid!
-- PRECONDITIONS: list of [Coordinates] should also include new piece (now only positions of opposite colors)!
makeTurn :: Color -> [Coordinates] -> Board -> Board
makeTurn color coords board = foldl replaceCell board coords
    where replaceCell board coordinates = replaceCellInBoard coordinates (FilledCell color) board

-- color is a color of current player
isLegalPath :: Color -> [Cell] -> Bool
isLegalPath currentColor (Blank:xs) = inverseColorExistsOnPath && restPathIsValid

                    where inverseColorExistsOnPath = not . null $ invertColorPath
                          invertColorPath = takeWhile invertColorExistsInCell xs
                          restPath = dropWhile invertColorExistsInCell xs 
                          restPathIsValid = ((not . null) restPath) && (head restPath == (FilledCell currentColor))
                          invertColorExistsInCell (FilledCell c) = invertColor currentColor == c
                          invertColorExistsInCell Blank = False
isLegalPath _ _ = False


generatePathOnBoard :: Board -> Coordinates -> Direction -> [(Coordinates, Cell)]
generatePathOnBoard board coordinates direction = map (\c -> (c, getCellFromBoard board c)) 
                                                  $ generatePath coordinates direction

generatePath :: Coordinates -> Direction -> [Coordinates]
generatePath coordinates direction = takeWhile isInBoard path
    where path = iterate (moveInDirection direction) coordinates


-- run application and display initial board
main :: IO ()
main = do
    putStr $ boardToString intialBoard
    print $ getLegalMoves intialBoard Black

-- 0) GAME STATE = BOARD, COLOR (print GAME STATE)
-- 1) getLegalMoves and display to the user
-- 2) user selects one of the suggested moves, prints error message if move is invalid
-- 3) if move is valid, then call getCellsToUpdate using the selected coordinates
-- 4) create new board by calling makeTurn using list of cells from 4)

data GameState = GameState Board Color

gameLoop :: IO ()
gameLoop = undefined

gameStateToString :: GameState -> IO ()
gameStateToString = undefined




