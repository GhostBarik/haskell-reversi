import Data.List

-- Cell on the board 
data Cell = Blank | FilledCell Color deriving Show
data Color = Black | White deriving Show

-- board represented as two-dimensional list
type Board = [[Cell]]

-- type aliases for common data types for better readability
type Position = Int
type Coordinates = (Position, Position)

-- initial board state for our game
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

-- converts the board to pretty-printable String
boardToString :: Board -> String
boardToString = concat . map rowsToString
    where rowsToString = (++ "\n") . intersperse ',' . rowToString
          rowToString  = map cellToChar

-- converts the cell to pretty-printable Character
cellToChar :: Cell -> Char
cellToChar Blank = '_'
cellToChar (FilledCell Black) = 'x'               
cellToChar (FilledCell White) = 'o'

-- put new cell into board on the specific position
replaceCellInBoard :: Board -> Coordinates -> Cell -> Board
replaceCellInBoard b (y,x) c = undefined -- TODO: implement

-- get cell (with specific {X,Y} coordinates) from board 
getCellFromBoard :: Board -> Coordinates -> Cell
getCellFromBoard b (y, x) = b !! y !! x

--      0    1    2
-- l => A -> B -> C -> []
-- drop 2 l =>  C -> []
-- take 2 l =>  A -> B -> []

-- replace element in list by new element (in specific position)
putElementToList :: Position -> a -> [a] -> [a]
putElementToList position newElement l = leftPartOfTheList ++ [newElement] ++ rightPartOfTheList
    where leftPartOfTheList  = take position l
          rightPartOfTheList = drop (position+1) l

-- run application and print initial board
main :: IO ()
main = putStr $ boardToString intialBoard


