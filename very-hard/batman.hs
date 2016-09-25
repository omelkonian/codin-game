import System.IO
import Control.Monad
import Debug.Trace (trace)

type Range = (Int, Int)
type Position = Range
type Output = (Int -> IO ())

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  input_line <- getLine
  let wh = words input_line
  let w = read (wh!!0) :: Int -- width of the building.
  let h = read (wh!!1) :: Int -- height of the building.
  _ <- getLine
  hPutStrLn stderr $ show w ++ " " ++ show h
  input_line <- getLine
  let wh = words input_line
  let x0 = read (wh!!0) :: Int -- width of the building.
  let y0 = read (wh!!1) :: Int -- height of the building.
  _ <- getLine
  (xRange, yRange) <- initialRanges (w, h) (x0, y0)
--   putStrLn "0 0"
--   _ <- getLine
  (x, y) <- trace ("Ranges: " ++ show xRange ++ " " ++ show yRange) execute xRange yRange
  hPutStrLn stderr $ "FINAL: " ++ show (x,y)
  putStrLn $ show x ++ " " ++ show y
  return ()

initialRanges :: Range -> Position -> IO (Range, Range)
-- initialRanges (w, h) _ = return ((0, w-1), (0, h-1))
initialRanges (w, h) (x0, y0) = do
  bombDirY <- (x0, y0) ~> (x0, 0)
  bombDirX <- (x0, 0) ~> (0, 0)
  hPutStrLn stderr $ "Dirs: " ++ bombDirX ++ " - " ++ bombDirY
  let xRange = if bombDirX == "COLDER" then ((x0`div`2) + 1, w-1) else if bombDirX == "WARMER" then (0, (x0-1)`div`2) else (0, 0)
  let yRange = if bombDirY == "COLDER" then ((y0`div`2) + 1, h-1) else if bombDirY == "WARMER" then (0, (y0-1)`div`2) else (0, 0)
  _ <- (0, 0) ~> (fst xRange, fst yRange)
  return (xRange, yRange)

execute :: Range -> Range -> IO Position
execute (xMin, xMax) (yMin, yMax) = do
  x <- binarySearch (xMin, xMax) (\value -> putStrLn $ show value ++ " " ++ show yMin) False False
  hPutStrLn stderr $ "X: " ++ show x
  y <- binarySearch (yMin, yMax) (\value -> putStrLn $ show x ++ " " ++ show value) False True
  hPutStrLn stderr $ "Y: " ++ show y
  return (x, y)

binarySearch :: Range -> Output -> Bool -> Bool -> IO Int
binarySearch (lo, hi) printFunction fromHi greed
  | lo == hi = return lo
  | greed && (count (lo, hi) == 2) = return $ if fromHi then lo else hi
  | otherwise = do
      hPutStrLn stderr $ show lo ++ "..." ++ show hi
      printFunction target
      bombDir <- getLine
      hPutStrLn stderr $ "BombDir: " ++ bombDir
      hPutStrLn stderr $ "C: " ++ show coldRange
      hPutStrLn stderr $ "W: " ++ show warmRange
    --   if (greed && ((count warmRange == 3) || (count coldRange == 3))) then do
    --     return middle
    --   else do
      case trace (bombDir++" C:"++show coldRange++" W:"++show warmRange) bombDir of
        "SAME" -> return middle
        "COLDER" -> do
          reset (if greed then initial`op`1 else initial) printFunction
          binarySearch coldRange printFunction fromHi greed
        "WARMER" -> binarySearch warmRange printFunction (not fromHi) greed
      where trunc = odd $ hi-lo+1
            middle = (hi+lo) `div` 2
            (initial, target) = if fromHi then (hi, lo) else (lo, hi)
            coldRange = if fromHi then (middle+1, hi) else (if greed then lo+1 else lo, if trunc then middle-1 else middle)
            warmRange = if fromHi then (lo, if trunc then middle-1 else middle) else (middle+1, hi)
            op = if fromHi then (-) else (+)

infix 2 ~>
(~>) :: Position -> Position -> IO String
(~>) (x0, y0) (x, y)
  | (x0 == x) && (y0 == y) = return ""
  | otherwise = do
      putStrLn $ show x ++ " " ++ show y
      getLine

count :: Range -> Int
count (lo, hi) = (hi - lo) + 1

reset :: Int -> Output -> IO ()
reset index printFunction = do
  printFunction index
  _ <- getLine
  return ()
