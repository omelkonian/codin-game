import Data.Set (fromList, Set)

main :: IO ()
main = do
    let morse = formatMorse ".-..-.---.-..-.-.."
    print morse
    let dict = ["ABC", "DEF", "GHCI"]

    print $ wordsToDict dict

getNumber :: String -> Int
getNumber []
getNumber ((c, accu) : rest) = getNumber0
--
-- getNumber0 :: String -> Int -> Int
-- getNumber0 [] accu = accu
-- getNumber0 (c:cs) accu =

formatMorse :: String -> [(Char, Int)]
formatMorse [] = [(' ', 1)]
formatMorse (c:cs) = (c, 1) : formatMorse cs

wordsToDict :: [String] -> Set String
wordsToDict ws = fromList $ map wordToMorse ws

wordToMorse :: String -> String
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
    _ -> ""
