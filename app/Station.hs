import Geo.Computations
import Geo.Types
import Text.CSV

data Station = Station{id :: Int
                      , location :: Point}

loadStations :: IO ()
loadStations = do
    let fileName = "tools/statmap"
    input <- readFile fileName
    let csv = parseCSV fileName input
    either handleError doWork csv

handleError csv = putStrLn "error parsing"
doWork csv = (print.findOldest.tail) csv

findOldest :: [Record] -> Record
findOldest [] = []
findOldest xs = foldl1

(\a x -> if age x > age a then x else a) xs
    age [a,b] = toInt a

toInt :: String -> Int
toInt = read
    

nearestStation :: Double -> Double -> Int
nearestStation lat lon = do
    let loc = Point lat lon Nothing Nothing
-- mock
    1+2


