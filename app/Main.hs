import Data.List

-- cell on the board 
data Cell = Blank | FilledCell Color deriving Show
data Color = Black | White deriving Show

-- board represented as two-dimensional list
type Board = [[Cell]]

-- type aliases for common data types for better readability
type Position = Int
type Coordinates = (Position, Position)

-- initial board state for our game
-- TODO: implement factory function for initial state
intialBoard :: Board
intialBoard = [
    [Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank],
    [Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank],
    [Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank],
    [Blank, Blank, Blank, FilledCell White, FilledCell Black, Blank, Blank, Blank],
    [Blank, Blank, Blank, FilledCell Black, FilledCell White, Blank, Blank, Blank],
    [Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank],
    [Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank],
    [Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank]]

-- convert the board to pretty-printable String
boardToString :: Board -> String
boardToString = concat . map rowToString
    where rowToString    = (++ "\n") . intersperse ',' . cellsToString
          cellsToString  = map cellToChar

-- converts the cell to pretty-printable Character
cellToChar :: Cell -> Char
cellToChar Blank = '_'
cellToChar (FilledCell Black) = 'x'               
cellToChar (FilledCell White) = 'o'

-- put the new cell into board on the specific position
replaceCellInBoard :: Board -> Coordinates -> Cell -> Board
replaceCellInBoard board (y,x) cell = undefined -- TODO: implement

-- get the specific cell (with specific {X,Y} coordinates) from board 
getCellFromBoard :: Board -> Coordinates -> Cell
getCellFromBoard board (y, x) = board !! y !! x

-- replace element in list by new element (in specific position)
-- TODO: use splitAt function!
replaceCellInList :: Position -> Cell -> [Cell] -> [Cell]
replaceCellInList position newElement l = 
    let (left, (_:right)) = splitAt position l -- split list and remove element on given position
    in left ++ [newElement] ++ right           -- put new element and merge list back

-- run application and display initial board
main :: IO ()
main = do
    putStr $ boardToString intialBoard


