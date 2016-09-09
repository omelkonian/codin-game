import System.IO
import Control.Monad
import Data.Map
import Data.Array.IO
import Data.IORef
import Data.Char


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    input_line <- getLine
    let l = read input_line :: Int
    input_line <- getLine
    let height = read input_line :: Int
    text <- getLine
    let width = 27 * l
    let dict = fromList [('A', 0),('B',l),('C',2*l),('D',3*l),('E',4*l),('F',5*l),('G',6*l),
                        ('H',7*l),('I',8*l),('J', 9*l),('K',10*l),('L',11*l),('M',12*l),('N',13*l),
                        ('O',14*l),('P',15*l),('Q',16*l),('R',17*l),('S', 18*l),('T',19*l),
                        ('U',20*l),('V',21*l),('W',22*l),('X',23*l),('Y',24*l),('Z',25*l),('?',26*l)]
    -- Create 2D array containing all the letters
    letters <- newArray ((1,1), (height,width)) ' ' :: IO (IOArray (Int, Int) Char)
    -- Copy array of letters from input
    x <- newIORef 1
    y <- newIORef 1
    replicateM height $ do
        row <- getLine
        writeIORef y 1
        replicateM width $ do
            x0 <- readIORef x
            y0 <- readIORef y
            writeArray letters (x0, y0) (row !! (y0 - 1))
            modifyIORef y (+ 1)
            return ()
        modifyIORef x (+ 1)
        return ()

    -- Initialize answer array
    let answerLen = (length text) * l
    answer <- newArray ((1,1), (height,answerLen)) '$' :: IO (IOArray (Int, Int) Char)

    -- Fill it according to input text
    r <- newIORef 0
    replicateM (length text) $ do
        r0 <- readIORef r
        let c = toUpper $ text !! r0
        let index = if (member c dict) then (maybe (26*l) id $ (Data.Map.lookup c dict)) else 26*l
        i <- newIORef 1
        j <- newIORef 1
        replicateM height $ do
            writeIORef j 1
            replicateM l $ do
                i0 <- readIORef i
                j0 <- readIORef j
                r0 <- readIORef r
                ch <- readArray letters (i0, index + j0)
                writeArray answer (i0, j0 + r0*l) ch
                modifyIORef j (+ 1)
                return ()
            modifyIORef i (+ 1)
            return ()
        modifyIORef r (+ 1)
        return ()

    -- Output answer
    writeIORef x 1
    writeIORef y 1
    replicateM height $ do
        writeIORef y 1
        replicateM answerLen  $ do
            x0 <- readIORef x
            y0 <- readIORef y
            ch <- readArray answer (x0, y0)
            putStr [ch]
            modifyIORef y (+ 1)
            return ()
        putStrLn ""
        modifyIORef x (+ 1)
        return ()
    return ()
