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
    let startState = GameState { currentPlayer = Red
                                 , board = board
                                 , humanPlayer = Red
                                 , cpuPlayer = Blue
                               }

    -- Red player starts the game first.
    let endState = playGame startState
    return ()

playGame :: GameState -> IO()
playGame state =
    case (getVictor state) of
        Just color -> case color of
            Red -> putStrLn "Red player wins!"
            Blue -> putStrLn "Blue player wins!"
        Nothing -> do
            let nextState = makeMove state
            putStrLn (sprintBoard (board nextState))
            playGame nextState

makeMove :: GameState -> GameState
makeMove state = state

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
longestRepetition color row = do
    let validRepetitions = any (\l -> (all (\x -> x == Just color) l)) (separateIntoRepetitions row)

    if (length validRepetitions) == 0 then
        0
    else
        (fold (\a b -> if a >= b then a else b)
            (map (\x -> length x) validRepetitions))

separateIntoRepetitions :: [a] -> [[a]]
separateIntoRepetitions list = separateIntoRepetitionsRec [[(first list)]] (tail list)

separateIntoRepetitionsRec :: [[a]] -> [a] -> [[a]]
separateIntoRepetitionsRec repetitions list =
    if (length list) != 0 then
        if (head list) == (last2d repetitions) then
            separateIntoRepetitionsRec (append (append (head list) (last2d repetitions)) (popLast repetitions)) (tail list)
        else
            separateIntoRepetitionsRec (append [(head list)] (repetitions)) (tail list)
    else
        repetitions
    
last2d :: [[a]] -> a
last2d list2d = (last (last list2d)

popLast :: [a] -> [a]
popLast list = (reverse (tail (reverse list)))

getVictor :: GameState -> Maybe Color
getVictor state =
    let
        redVictor = isVerticalVictor Red (board state)
            || isHorizontalVictor Red (board state)
            || isDiagonalVictor Red (board state)

        blueVictor = isVerticalVictor Blue (board state)
            || isHorizontalVictor Blue (board state)
            || isDiagonalVictor Blue (board state)
    in
        if redVictor then
            Just Red
        else
            if blueVictor then
                Just Blue
            else
                Nothing

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
    line <- getLine
    let num = readMaybe line :: Maybe Int

    case num of 
        Just d  -> return (d)
        Nothing -> getNextMove board

nextTurn :: Color -> Color
nextTurn current =
    if current == Red then
        Blue
    else
        Red
