import System.IO
import Control.Monad
import Data.List

prInfo x m = hPutStrLn stderr $ m ++ (show x)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let input = words input_line
    let w = read (input!!0) :: Int
    let h = read (input!!1) :: Int
    input_line <- getLine
    let n = read input_line :: Int
    input_line <- getLine
    let input = words input_line
    let x0 = read (input!!0) :: Int
    let y0 = read (input!!1) :: Int

    prInfo w "Width: "
    prInfo h "Height: "

    let space =((0,w-1),(0,h-1)) -- bounds: ((xmin,xmax),(ymin,ymax))
    let factor = max (w `div` 2) (h `div` 2)
    loop (x0, y0) factor (w-1) (h-1) "" 5 space

loop (x,y) l w h lastDir repeats space = do
    input_line <- getLine
    let dir = input_line :: String
    prInfo dir "Dir: "
    let ns = crop space (x,y) dir
    prInfo ns "Space: "

    let j = getJump dir (x, y) l
    prInfo j "Jump1: "

    let jump = sanitize j (fst$fst ns,snd$fst ns) (fst$snd ns,snd$snd ns)
    prInfo jump "Jump2: "

    putStrLn $ (show $ fst jump) ++ " " ++ (show $ snd jump)

    if (lastDir /= "" && opposite dir lastDir) then
        loop jump (max (l `div` repeats) 1) w h dir (repeats+1) ns
    else
        loop jump (max (l-1) 1) w h dir 2 ns

getJump dir (x, y) l = case dir of
    "U" -> (x,y-l)
    "UR" -> (x+l,y-l)
    "R" -> (x+l,y)
    "DR" -> (x+l,y+l)
    "D" -> (x,y+l)
    "DL" -> (x-l,y+l)
    "L" -> (x-l,y)
    "UL" -> (x-l,y-l)

crop ((xmin,xmax),(ymin,ymax)) (x0,y0) dir = case dir of
    "U"  -> ((xmin,xmax),(min ymin (y0-1),min ymax (y0-1)))
    "D"  -> ((xmin,xmax),(max ymin (y0+1),max ymax (y0+1)))
    "L"  -> ((min xmin (x0-1),min xmax (x0-1)),(ymin,ymax))
    "R"  -> ((max xmin (x0+1),max xmax (x0+1)),(ymin,ymax))
    "UR" -> ((max xmin (x0+1),max xmax (x0+1)),(min ymin (y0-1),min ymax (y0-1)))
    "UL" -> ((min xmin (x0-1),min xmax (x0-1)),(min ymin (y0-1),min ymax (y0-1)))
    "DR" -> ((max xmin (x0+1),max xmax (x0+1)),(max ymin (y0+1),max ymax (y0+1)))
    "DL" -> ((min xmin (x0-1),min xmax (x0-1)),(max ymin (y0+1),max ymax (y0+1)))

opposite dir1 dir2 = elem (dir1,dir2) opp
                 ||  elem (dir2,dir1) opp
                 where opp = [("U","D"),("R","L"),("UR","DL"),("UL","DR")]

sanitize (x,y) (xmin,xmax) (ymin,ymax) =
    (max (min x xmax) xmin, max (min y ymax) ymin)

