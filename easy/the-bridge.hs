import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let road = read input_line :: Int -- the length of the road before the gap.
    input_line <- getLine
    let gap = read input_line :: Int -- the length of the gap.
    input_line <- getLine
    let platform = read input_line :: Int -- the length of the landing platform.\

    hPutStrLn stderr (show road ++ " " ++ show gap ++ " " ++ show platform)

    loop road gap platform

loop :: Int -> Int -> Int -> IO ()
loop road gap platform = do
    input_line <- getLine
    let speed = read input_line :: Int -- the motorbike's speed.
    input_line <- getLine
    let coordx = read input_line :: Int -- the position on the road of the motorbike.

    hPutStrLn stderr (show speed ++ " " ++ show coordx)

    if (coordx >= road + gap) then  loop2 else
        (if (speed <= gap) then putStrLn "SPEED" else
            (if (coordx + speed > road) then putStrLn "JUMP" else
                (if (speed > gap + 1) then putStrLn "SLOW" else
                    putStrLn "WAIT")))


    loop road gap platform

loop2 = do
    putStrLn "SLOW"
