import Data.List

-- cell on the board 
data Cell = Blank | FilledCell Color deriving Show
data Color = Black | White deriving Show

-- board represented as two-dimensional list
type Board = [[Cell]]

-- type aliases for common data types for better readability
type Position = Int
type Coordinates = (Position, Position)

-- the dimensions of board
boardWidth  = 8
boardHeight = 8

-- initial board state for our game
-- create initial board with size 8x8
intialBoard :: Board
intialBoard = 
      replaceCellInBoard (3, 3) (FilledCell White) 
    . replaceCellInBoard (3, 4) (FilledCell Black) 
    . replaceCellInBoard (4, 3) (FilledCell Black)
    . replaceCellInBoard (4, 4) (FilledCell White)
       $ blankBoard 
    where blankBoard = replicate boardHeight row
          row        = replicate boardWidth Blank

-- convert the board to pretty-printable String
boardToString :: Board -> String
boardToString = concatMap $ (++ "\n") . (intersperse ',') . (map cellToChar)

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

-- run application and display initial board
main :: IO ()
main = do
    putStr $ boardToString intialBoard



