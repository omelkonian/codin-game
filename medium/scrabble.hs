import System.IO
import Control.Monad
import TrieMap

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    (dict, letters) <- parse

    -- dictTrie dict

    return ()


parse :: IO ([String], String)
parse = do
    s <- getContents
    let aux xs [x] = (xs, x)
        aux xs (y:ys) = aux (y:xs) ys
    return aux [] (drop 1 (lines s))

