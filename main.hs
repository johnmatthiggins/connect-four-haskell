data Color = Red | Blue

main :: IO ()
main = do
    let board = newBoard
    print (board :: [[Maybe Color c]])
    return ()

newBoard = [
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]

-- find valid spaces
-- valid space
-- board
