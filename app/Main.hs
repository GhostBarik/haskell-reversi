import Data.List

data Cell = Blank | FilledCell CellColor deriving Show
data CellColor = Black | White deriving Show

type Board = [[Cell]]
type Position = Int
type Coordinates = (Position, Position)

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
    where rowsToString l = intersperse ',' (rowToString l) ++ "\n"
          rowToString l  = map cellToChar l

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


