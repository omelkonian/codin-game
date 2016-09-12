import Data.List (sortBy)
import Data.Function (on)

main :: IO ()
main = do
  let input = ["CCCTG","TGACA","CATGA"]
  let subseq = sortBy (compare `on` length) input
  let minSize = maximum $ map length subseq
  print $ check subseq minSize

check :: [String] -> Int -> Int
check sequences len =
  if any isFeasibleM $ mapM (place len) sequences
    then len
    else check sequences (len + 1)

isFeasibleM :: [String] -> Bool
isFeasibleM [] = True
isFeasibleM [_] = True
isFeasibleM (x:y:xs) =
  case isFeasible x y of
    Just s -> isFeasibleM (s:xs)
    Nothing -> False

isFeasible :: String -> String -> Maybe String
isFeasible [] [] = Just ""
isFeasible ('$':xs) (c:ys) =
  case isFeasible xs ys of
    Just s -> Just $ c:s
    Nothing -> Nothing
isFeasible (c:xs) ('$':ys) =
  case isFeasible xs ys of
    Just s -> Just $ c:s
    Nothing -> Nothing
isFeasible (x:xs) (y:ys) =
  case isFeasible xs ys of
    Just s -> if x /= y then Nothing else Just $ x:s
    Nothing -> Nothing

place :: Int -> String -> [String]
place size str = place0 size str 0 (size - length str + 1)

place0 :: Int -> String -> Int -> Int -> [String]
place0 _ _ _ 0 = []
place0 size str index iter =
  let
    len = length str
    fill = replicate (size - len) '$'
    (s1, s2) = splitAt index fill
  in (s1 ++ str ++ s2) : place0 size str (index+1) (iter-1)
