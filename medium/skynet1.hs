import System.IO
import System.Random
import Control.Monad
import Data.List
import Data.IORef

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let input = words input_line
    let n = read (input!!0) :: Int
    let l = read (input!!1) :: Int
    let e = read (input!!2) :: Int

    hPutStrLn stderr $ "#Links: " ++ (show l)

    list <- newIORef []
    replicateM l $ do
        input_line <- getLine
        let input = words input_line
        let n1 = read (input!!0) :: Int
        let n2 = read (input!!1) :: Int
        l0 <- readIORef list
        writeIORef list ((n1, n2):((n2, n1):l0))
        return ()

    exits <- newIORef []
    replicateM e $ do
        input_line <- getLine
        let ei = read input_line :: Int
        e0 <- readIORef exits
        writeIORef exits (ei:e0)
        return ()

    loop list exits n


loop l e n = do
    l0 <- readIORef l
    e0 <- readIORef e
    input_line <- getLine
    let si = read input_line :: Int

    let neighbors = [x | x <- [0..n], (elem (si, x) l0) || (elem (x, si) l0)]
    let exits = filter (\c -> (elem c e0)) neighbors

    toDel <- newIORef (0, 0)
    bool <- newIORef False
    if (exits /= []) then do
        if (elem (si, head exits) l0) then do
            writeIORef toDel (si, head exits)
        else do writeIORef bool True
    else do writeIORef bool True
    bool0 <- readIORef bool
    if bool0 then do
        r <- randomRIO (0, (length l0) - 1) :: IO Int
        writeIORef toDel $ l0!!r
    else return ()

    del <- readIORef toDel
    putStrLn $ (show $ fst del) ++ " " ++ (show $ snd del)

    writeIORef l $ delete del l0

    loop l e n
