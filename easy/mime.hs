import System.IO
import Control.Monad
import qualified Data.Map as Map
import Data.List.Split
import Data.IORef
import Data.Char


main :: IO()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let n = read input_line :: Int -- Number of elements which make up the association table.
    input_line <- getLine
    let q = read input_line :: Int -- Number Q of file names to be analyzed.

    table <- newIORef Map.empty
    replicateM n $ do
        input_line <- getLine
        let input = words input_line
        let ext = map toLower $ input!!0 -- file extension
        let mt = input!!1 -- MIME type.
        t <- readIORef table
        writeIORef table (Map.insert ext mt t)
        return ()
    t0 <- readIORef table

    replicateM q $ do
        fname <- getLine
        let extention = if (notElem '.' fname) then "" else map toLower $ head $ reverse (splitOn "." fname)
        if (extention == "" || Map.notMember extention t0) then putStrLn "UNKNOWN"
        else putStrLn $ maybe "UNKNOWN" id (Map.lookup extention t0)
        return ()
    return ()
