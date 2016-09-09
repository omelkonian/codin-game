import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let n = read input_line :: Int
    temps <- getLine

    let numbers = f $ words temps
    let positives = filter (>= 0) numbers
    let negatives = map abs $ filter (< 0) numbers

    if (n == 0) then putStrLn "0" else (main2 positives negatives)


main2 :: [Int] -> [Int] -> IO()
main2 positives negatives = do
    let min1 = if (0 ==  length positives) then 5526 else (minimum positives)
    let min2 = if (0 ==  length negatives) then 5526 else (minimum negatives)

    (if (min1 == min2) then putStrLn (show min1) else
        (if (min1 < min2) then putStrLn (show min1) else
            putStrLn $ "-" ++ (show min2)))

f :: [String] -> [Int]
f = map read
