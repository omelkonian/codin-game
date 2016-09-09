import System.IO
import Control.Monad
import Data.IORef
import Data.Char
import Data.List
import Data.List.Split

toBin :: Int -> [Int]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise = toBin (n `div` 2) ++ [1]
toBinary x = (replicate (7 - (length  bin)) 0) ++ bin where bin = toBin x

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    text <- getLine
    let t = (group . concat) (map (toBinary . ord) text)
    let len = length t

    index <- newIORef 0
    replicateM len $ do
        i <- readIORef index
        let list = t !! i
        let l = length list
        if ((head $ list) == 0) then
            putStr $ "00 " ++ (map intToDigit $ replicate l 0)
        else
            putStr $ "0 " ++ (map intToDigit $ replicate l 0)
        if (i /= len-1) then putStr " " else putStr ""
        modifyIORef index (+ 1)
        return ()
    return ()

