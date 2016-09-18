import qualified Data.Map as Map
import Data.Map (Map)
import Data.Array (listArray, (!))
import Data.Ix (range)
import Data.Maybe (fromMaybe)
import Control.Monad (replicateM)

type MorseCode = String
type Dictionary = Map MorseCode Int

main :: IO ()
main = do
    morse <- getLine
    input_line <- getLine
    let n = read input_line :: Int
    ws <- replicateM n getLine
    let res = getNumber morse $ wordsToDict ws
    print res

getNumber :: MorseCode -> Dictionary -> Int
getNumber morse dict = go len
  where len = length morse

        go (-1) = 0
        go 0 = 1
        go n = sum [calc n k | k <- [n,n-1..smallest]]
          where smallest = max 0 (n-80)

        calc index subsize =
          if k /= 0 then k * arr ! (subsize - 1) else 0
          where s = substring subsize index morse
                k = count s dict

        bounds = (-1, len)
        arr = listArray bounds [go x | x <- range bounds]

substring :: Int -> Int -> String -> String
substring start end str = take (end - start + 1) $ drop (start - 1) str

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
