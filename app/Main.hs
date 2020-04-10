import Data.List

-- cell on the board 
data Cell = Blank | FilledCell Color deriving (Show, Eq)
data Color = Black | White deriving (Show, Eq)

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
intialBoard = foldl (.) id fillFunctions blankBoard 
    where fillFunctions = [replaceCellInBoard (y, x) color | 
                          y <- [3,4], x <- [3,4], 
                          color <- if x == y then [FilledCell White] else [FilledCell Black]]
          blankBoard    = replicate boardHeight row
          row           = replicate boardWidth Blank

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

getLegalMoves :: Board -> Color -> [Coordinates]
getLegalMoves board color = filter (isMoveLegal board color) [(y, x) | y <- [0..boardHeight], x <- [0..boardWidth]]

-- generatePathOnBoard :: Board -> Coordinates -> Direction -> [Cell]
-- function to check if a move on a given Position with a given FilledCell is legal
isMoveLegal :: Board -> Color -> Coordinates -> Bool
isMoveLegal board color coordinates = 
    any (isLegalPath color) [generatePathOnBoard board coordinates direction | 
         direction <- [DLeft, DRight, DTop , DBottom, DTopLeft, DTopRight, DBottomLeft, DBottomRight]]

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

data Direction = DLeft | DRight | DTop | DBottom | DTopLeft | DTopRight | DBottomLeft | DBottomRight

moveInDirection :: Direction -> Coordinates -> Coordinates
moveInDirection DLeft (y, x) = (y, x - 1)
moveInDirection DRight (y, x) = (y, x + 1)
moveInDirection DTop (y, x) = (y - 1, x)
moveInDirection DBottom (y, x) = (y + 1, x)
moveInDirection DTopLeft (y, x) = (y - 1, x - 1)
moveInDirection DTopRight (y, x) = (y - 1, x + 1)
moveInDirection DBottomLeft (y, x) = (y + 1, x - 1)
moveInDirection DBottomRight (y, x) = (y + 1, x + 1)

generatePath :: Coordinates -> Direction -> [Coordinates]
generatePath coordinates direction = takeWhile isInBoard path
    where path = iterate (moveInDirection direction) coordinates

generatePathOnBoard :: Board -> Coordinates -> Direction -> [Cell]
generatePathOnBoard board coordinates direction = map (getCellFromBoard board) $ generatePath coordinates direction

isOutOfBoard :: Coordinates -> Bool
isOutOfBoard (y, x) = y < 0 || y >= boardHeight || x < 0 || x >= boardWidth

isInBoard :: Coordinates -> Bool
isInBoard = not . isOutOfBoard

invertColor :: Color -> Color
invertColor White = Black
invertColor Black = White


-- taken: Color = C
-- inverseColor :: Color -> Color
-- 1. first element should be Blank
-- 2. inverseColor should take 1..N next positions in list
-- 3. sequence in (2) should end with C

-- need to check all 8 directions

-- run application and display initial board
main :: IO ()
main = do
    putStr $ boardToString intialBoard



