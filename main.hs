import Text.Printf
import Data.List

data Color = Red | Blue deriving Show

main :: IO ()
main = do
    let board = (newBoard 6 7) in printf "%s\n" (sprintBoard board)
    return ()

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
    || isHorizontalVictor color board
    || isDiagonalVictor color board

isVerticalVictor :: Color -> [[Maybe Color]] -> Bool
isVerticalVictor color board =
    any (\x -> (longestRepetition x) >= 4) (transpose board)

isHorizontalVictor :: Color -> [[Maybe Color]] -> Bool
isHorizontalVictor color board =
    any (\x -> (longestRepetition color x) >= 4) board

isDiagonalVictor :: Color -> [[Maybe Color]] -> Bool
isDiagonalVictor color board =
    any (\x -> (longestRepetition color x) >= 4) (
            (filter (\x -> (length x) >= 4) (getDiagonalSlices board))
        )

boardPoints :: [[Maybe Color]] -> [(Int, Int)]
boardPoints board = matrixPoints (length board) (length (transpose board))

matrixPoints :: Int -> Int -> [(Int, Int)]
matrixPoints xlen ylen = [(xlen, ylen) | x <- [0..xlen], y <- [0..ylen]]

getDiagonalSlices :: [[Maybe Color]] -> [[Maybe Color]]
getDiagonalSlices board = [[Just Red]]
