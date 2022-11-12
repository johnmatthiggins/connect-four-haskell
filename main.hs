import Text.Printf
import Text.Read
import Data.List

data Color = Red | Blue deriving (Show, Eq)
data GameState = GameState { currentPlayer :: Color
                           , board :: [[Maybe Color]]
                           , cpuPlayer ::  Color
                           , humanPlayer :: Color
                           }

main :: IO ()
main = do
    let board = (newBoard 6 7)

    -- Red player starts the game first.
    playGame state Red
    return ()

playGame :: GameState -> Color -> IO()
playGame state player =
    case (isVictor player state . board) of
        Just color -> case color of
            Red -> putStrLn "Red player wins!"
            Blue -> putStrLn "Blue player wins!"
        Nothing -> do
            let nextState = makeMove state
            putStrLn (sprintBoard (nextState . board))
            playGame nextState (nextTurn color)

makeMove :: GameState -> GameState

newBoard :: Int -> Int -> [[Maybe Color]]
newBoard x y = replicate x (replicate y Nothing)

sprintBoard :: [[Maybe Color]] -> String
sprintBoard board  = intercalate "\n" (map sprintRow board)

showSquare :: Maybe Color -> String
showSquare square = case square of
    Nothing    -> " "
    Just color -> case color of 
        Red  -> "R"
        Blue -> "B"

sprintRow :: [Maybe Color] -> String
sprintRow spaces =
    intercalate "" ["|", (
        intercalate "|" (map showSquare spaces)), "|"]

isSpaceFilledWith :: Color -> Maybe Color -> Bool
isSpaceFilledWith color space =
    case space of 
        Just c  -> c == color
        Nothing -> False

-- Fix this later
longestRepetition :: Color -> [Maybe Color] -> Int
longestRepetition color row = 3

isVictor :: Color -> [[Maybe Color]] -> Bool
isVictor color board =
    isVerticalVictor color board
        || isHorizontalVictor color board || isDiagonalVictor color board

isVerticalVictor :: Color -> [[Maybe Color]] -> Bool
isVerticalVictor color board =
    any (\x -> (longestRepetition color x) >= 4) (transpose board)

isHorizontalVictor :: Color -> [[Maybe Color]] -> Bool
isHorizontalVictor color board =
    any (\x -> (longestRepetition color x) >= 4) board

isDiagonalVictor :: Color -> [[Maybe Color]] -> Bool
isDiagonalVictor color board =
    any (\x -> (longestRepetition color x) >= 4) (
            (filter (\x -> (length x) >= 4) (getDiagonalSlices board)))

boardPoints :: [[Maybe Color]] -> [(Int, Int)]
boardPoints board = matrixPoints (length board) (length (transpose board))

matrixPoints :: Int -> Int -> [(Int, Int)]
matrixPoints xlen ylen = [(xlen, ylen) | x <- [0..xlen], y <- [0..ylen]]

getDiagonalSlices :: [[Maybe Color]] -> [[Maybe Color]]
getDiagonalSlices board = [[Just Red]]

getNextMove :: [[Maybe Color]] -> IO(Int)
getNextMove board = do
    putStr "Pick a column: "
    let num = (getLine) >>= readMaybe

    case num of 
        Just d  -> return (d)
        Nothing -> getNextMove board

nextTurn :: Color -> Color
nextTurn current =
    if current == Red then
        Blue
    else
        Red
