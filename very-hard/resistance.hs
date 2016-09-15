import qualified Data.Map as Map
import Data.List (inits, tails)
import Data.Map (Map)
import Data.Array (listArray, (!))
import Data.Ix (range)
import Data.Maybe (fromMaybe)
import Control.Monad (replicateM)
import Debug.Trace (trace)

type MorseCode = String
type Dictionary = Map MorseCode Int
type Subsequences = Map (Int, Int) String

main :: IO ()
main = do
    morse <- getLine
    input_line <- getLine
    let n = read input_line :: Int
    ws <- replicateM n getLine
    let res = getNumber morse $ wordsToDict ws
    print res

getNumber :: MorseCode -> Dictionary -> Int
getNumber morse dict = go len -- -1??
  where len = length morse

        go (-1) = 0
        go 0 = 1
        go n = sum [calc n k | k <- [n,n-1..0]]

        calc index subsize =
          if k /= 0 then k * arr ! (subsize - 1) else 0
          where s = substring subsize index morse
                k = count s dict

        -- calc index subsize = trace (show index ++ ":" ++ show subsize) $
        --   case fetch_dict of
        --     Just k -> count k dict * fetch_array
        --     Nothing -> 0
        --   where fetch_dict = Map.lookup (subsize, index) subs
        --         fetch_array = arr ! (subsize - 1)

        bounds = (-1, len)
        arr = listArray bounds [go x | x <- range bounds]
        subs = trace (show $ subseq morse) subseq morse

substring :: Int -> Int -> String -> String
substring start end str = take (end - start + 1) $ drop (start - 1) str

continuousSubsequences :: [a] -> [[a]]
continuousSubsequences = filter (not . null) . concatMap tails . inits

subseq :: String -> Subsequences
subseq xs = Map.fromList $ map (\(i, str) -> ((head i, last i), str)) $ zip indices s
  where s = continuousSubsequences xs
        indices = continuousSubsequences [0..]

count :: MorseCode -> Dictionary -> Int
count morse dict = fromMaybe 0 (Map.lookup morse dict)

wordsToDict :: [String] -> Dictionary
wordsToDict ws = dict
  where ms = map wordToMorse ws :: [MorseCode]
        dict = Map.fromListWith (+) [(m, 1) | m <- ms] :: Dictionary

wordToMorse :: String -> MorseCode
wordToMorse = foldr ((++) . charToMorse) ""

charToMorse :: Char -> String
charToMorse c = case c of
    'A' -> ".-"
    'B' -> "-..."
    'C' -> "-.-."
    'D' -> "-.."
    'E' -> "."
    'F' -> "..-."
    'G' -> "--."
    'H' -> "...."
    'I' -> ".."
    'J' -> ".---"
    'K' -> "-.-"
    'L' -> ".-.."
    'M' -> "--"
    'N' -> "-."
    'O' -> "---"
    'P' -> ".--."
    'Q' -> "--.-"
    'R' -> ".-."
    'S' -> "..."
    'T' -> "-"
    'U' -> "..-"
    'V' -> "...-"
    'W' -> ".--"
    'X' -> "-..-"
    'Y' -> "-.--"
    'Z' -> "--.."
