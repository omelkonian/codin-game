import System.IO
import Control.Monad
import Data.IORef
import Data.Array
import Data.List
import Data.List.Split
import Data.IORef

prs x = hPutStrLn stderr $ show x
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let width = read input_line :: Int -- the number of cells on the X axis
    input_line <- getLine
    let height = read input_line :: Int -- the number of cells on the Y axis

    hPutStr stderr "Width: "
    prs width
    hPutStr stderr"Height: "
    prs height

    str <- newIORef ""
    replicateM height $ do
        line <- getLine
        str0 <- readIORef str
        writeIORef str $ str0 ++ line
        return ()
    str0 <- readIORef str

    let s = chunksOf 1 str0
    let h = height-1
    let w = width-1
    let grid = listArray ((0, 0), (h, w)) s

    i <- newIORef 0
    j <- newIORef 0
    replicateM height $ do
        i0 <- readIORef i
        writeIORef j 0
        replicateM width $ do
            j0 <- readIORef j

            -- Current node
            if (grid!(i0,j0) == "0") then do
                putStr $ (show j0) ++ " " ++ (show i0) ++ " "

                -- Right neighbor
                ri <- newIORef (-1)
                rj <- newIORef (-1)
                bool <- newIORef False
                jj <- newIORef (j0+1)
                replicateM (w-j0) $ do
                    jj0 <- readIORef jj
                    bool0 <- readIORef bool

                    if (not bool0) then do
                        if (grid!(i0,jj0) == "0") then do
                            writeIORef bool True
                            writeIORef ri i0
                            writeIORef rj jj0
                        else do
                            modifyIORef jj (+ 1)
                    else do
                        modifyIORef jj (+ w)
                    return ()
                ri0 <- readIORef ri
                rj0 <- readIORef rj
                putStr $ (show rj0) ++ " " ++ (show ri0) ++ " "

                -- Bottom neighbor
                bi <- newIORef (-1)
                bj <- newIORef (-1)
                writeIORef bool False
                ii <- newIORef (i0+1)
                replicateM (h-i0) $ do
                    ii0 <- readIORef ii
                    bool0 <- readIORef bool

                    if (not bool0) then do
                        if (grid!(ii0,j0) == "0") then do
                            writeIORef bool True
                            writeIORef bi ii0
                            writeIORef bj j0
                        else do
                            modifyIORef ii (+ 1)
                    else do
                        modifyIORef ii (+ h)


                    return ()
                bi0 <- readIORef bi
                bj0 <- readIORef bj
                putStrLn $ (show bj0) ++ " " ++ (show bi0) ++ " "
            else
                writeIORef j j0



            modifyIORef j (+ 1)
            return ()

        modifyIORef i (+ 1)
        return ()
    return ()
