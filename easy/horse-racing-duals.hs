import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import Data.IORef

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    list <- getInts
    let m = findMinumum (drop 1 list)
    putStrLn (show m)


getInts :: IO [Int]
getInts = do
    s <- getContents
    return (map (\x -> (read x)::Int) $ lines s)


findMinumum :: [Int] -> Int
findMinumum ls = minimum $ map (\(x,y) -> abs(x - y)) $ pairs0 (sort ls)


pairs :: [Int] -> [(Int, Int)]
pairs [] = []
pairs (x:xs) = map (\y -> (x, y)) xs ++ pairs xs


pairs0 :: [Int] -> [(Int, Int)]
pairs0 [] = []
pairs0 [x] = []
pairs0 (x:(y:xs)) = (x,y):pairs0 (y:xs)
