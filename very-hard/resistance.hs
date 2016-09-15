import Data.Set (Set, fromList, member)
import Data.Array (listArray, (!))
import Data.Ix (range)
import Control.Monad (replicateM)

type MorseCode = String
type Dictionary = Set String

main :: IO ()
main = do
    input_line <- getLine
    let morse = input_line :: String
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
        go n = sum [calc n k | k <- [n,n-1..0]]

        calc index subsize =
          if member (substring subsize index morse) dict then
            arr ! (subsize - 1)
          else
            0

        bounds = (-1, len)
        arr = listArray bounds [go x | x <- range bounds]
        -- subs = listArray bounds []

substring :: Int -> Int -> String -> String
substring start end str = take (end - start + 1) $ drop (start - 1) str

wordsToDict :: [String] -> Dictionary
wordsToDict = fromList . map wordToMorse

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
