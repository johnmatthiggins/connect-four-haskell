data Color = Red | Blue deriving Show

main :: IO ()
main = do
    let board = newBoard in print board
    return ()

newBoard :: [[Maybe Color]]
newBoard = [
    (replicate 20 Nothing),
    (replicate 20 Nothing),
    (replicate 20 Nothing),
    (replicate 20 Nothing),
    (replicate 20 Nothing)]

-- find valid spaces
-- valid space
-- board
