import System.IO
import Control.Monad
import Data.Array.IO
import Data.IORef

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let input = words input_line
    let w = read (input!!0) :: Int -- number of columns.
    let h = read (input!!1) :: Int -- number of rows.

    grid <- newArray ((1,1), (h,w)) 0 :: IO (IOArray (Int, Int) Int)
    x <- newIORef 1
    y <- newIORef 1
    replicateM h $ do
        row <- getLine
        let r = words row
        writeIORef y 1
        replicateM w $ do
            x0 <- readIORef x
            y0 <- readIORef y
            let s = r !! (y0 - 1)
            let roomType = read s :: Int
            writeArray grid (x0, y0) roomType
            modifyIORef y (+ 1)
            return ()
        modifyIORef x (+ 1)
        return ()
    input_line <- getLine
    let ex = read input_line :: Int -- the coordinate along the X axis of the exit (not useful for this first mission, but must be read).
    loop grid

loop grid = do
    input_line <- getLine
    let input = words input_line
    let xi = read (input!!0) :: Int
    let yi = read (input!!1) :: Int
    let pos = input!!2

    room <- readArray grid (yi+1,xi+1)
    let dir = getDir pos room
    let x = if (dir == "D") then (yi+1) else yi
    let y = if (dir == "L") then (xi-1) else (if (dir == "R") then (xi+1) else xi)

    putStrLn((show y) ++ " " ++ (show x))

    loop grid


getDir pos p | elem p [1,3,7,8,9,12,13] = "D"
        | elem p [2,6] = if (pos == "LEFT") then "R" else "L"
        | p == 4 = if (pos == "TOP") then "L" else "D"
        | p == 5 = if (pos == "TOP") then "R" else "D"
        | p == 10 = "L"
        | otherwise = "R"
