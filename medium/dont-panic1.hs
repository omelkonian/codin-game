import System.IO
import Control.Monad
import Data.List
import Data.IORef

prs x = hPutStrLn stderr $ show x
prInfo x m = hPutStrLn stderr $ m ++ (show x)

replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let input = words input_line
    let nbfloors = read (input!!0) :: Int -- number of floors
    let width = read (input!!1) :: Int -- width of the area
    let nbrounds = read (input!!2) :: Int -- maximum number of rounds
    let exitfloor = read (input!!3) :: Int -- floor on which the exit is found
    let exitpos = read (input!!4) :: Int -- position of the exit on its floor
    let nbtotalclones = read (input!!5) :: Int -- number of generated clones
    let nbadditionalelevators = read (input!!6) :: Int -- ignore (always zero)
    let nbelevators = read (input!!7) :: Int -- number of elevators

    elevators <- newIORef $ replicate nbelevators 0
    replicateM nbelevators $ do
        input_line <- getLine
        let input = words input_line
        let elevatorfloor = read (input!!0) :: Int -- floor on which this elevator is found
        let elevatorpos = read (input!!1) :: Int -- position of the elevator on its floor

        l0 <- readIORef elevators
        writeIORef elevators $ replace elevatorfloor elevatorpos l0

        return ()
    elev <- readIORef elevators
    loop elev exitfloor exitpos

loop elev exitfloor exitpos = do
    prs $ elev

    input_line <- getLine
    let input = words input_line
    let clonefloor = read (input!!0) :: Int -- floor of the leading clone
    let clonepos = read (input!!1) :: Int -- position of the leading clone on its floor
    let direction = input!!2 -- direction of the leading clone: LEFT or RIGHT

    if (clonefloor == -1) then
        putStrLn "WAIT"
    else
        if (clonefloor == exitfloor) then
            if ((direction=="LEFT" && clonepos>exitpos) || (direction=="RIGHT" && clonepos<exitpos)) then
                putStrLn "WAIT"
            else
                putStrLn "BLOCK"
        else do
            let elevpos = elev!!clonefloor
            if ((direction=="LEFT" && clonepos>elevpos) || (direction=="RIGHT" && clonepos<elevpos) || clonepos==elevpos) then
                putStrLn "WAIT"
            else
                putStrLn "BLOCK"

    loop elev exitfloor exitpos
