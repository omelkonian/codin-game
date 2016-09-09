import System.IO
import Control.Monad
import Data.List.Split
import Data.IORef

data Defib = Defib {    idd :: Int,
                        name :: String,
                        address :: String,
                        contact :: String,
                        longitude :: Float,
                        latitude :: Float
                    } deriving (Show, Read)
data Coords = Coords {  lon :: Float,
                        lat :: Float}

toNum x = map (\c -> if (c == ',') then '.' else c) x
toRadian = (* (pi / 180))

distance :: Coords -> Coords -> Float
distance ca cb =
    (sqrt $ x^2 + y^2 ) * 6371
    where
        loa = toRadian $ lon ca
        lob = toRadian $ lon cb
        laa = toRadian $ lat ca
        lab = toRadian $ lat cb
        x = (lob - loa) * cos ((laa+lab)/2)
        y = (lab - laa)


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    input_line <- getLine
    let lon = input_line :: String
    let lo = read (toNum lon) :: Float
    input_line <- getLine
    let lat = input_line :: String
    let la = read (toNum lat) :: Float
    input_line <- getLine
    let n = read input_line :: Int

    dist <- newIORef 999999
    ans <- newIORef ""
    replicateM n $ do
        defib <- getLine
        let d = splitOn ";" defib
        -- Create record
        let newD = Defib {  idd= read (d!!0) :: Int,
                            name=d!!1,
                            address=d!!2,
                            contact=d!!3,
                            longitude= read (toNum $ d!!4) :: Float,
                            latitude= read (toNum $ d!!5)  :: Float
                         }
        -- Calculate distance
        let c1 = Coords { lon=lo, lat=la }
        let c2 = Coords { lon=longitude newD, lat=latitude newD }
        let dis = distance c1 c2

        dist0 <- readIORef dist
        if (dis < dist0) then do
            writeIORef dist dis
            writeIORef ans (name newD)
        else
            writeIORef dist dist0

        return ()

    ans0 <- readIORef ans
    putStrLn ans0
