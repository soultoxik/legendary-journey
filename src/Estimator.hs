{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Estimator where
import Types
import qualified Data.Text as T



animalsBase :: [Animal]
animalsBase = map (\(n, vals) -> makeAnimalRaw n vals) [
  ("chicken", [("co", 60 * 60 * 1000, 4.581)]),
  ("pigeon", [("co", 60 * 60 * 1000, 9.162)]),
  ("pig", [("co", 60 * 60 * 1000, 9.162), ("o3", 60 * 60 * 1000 * 3, 102), ("so2", 3 * 60 * 60 * 1000, 89)]),
  ("any living thing", [("no2", 0, 28.2)]),
  ("mouse", [("no2", 92 * 60 * 60 * 1000, 6.6)]),
  ("hamster", [("no2", 92 * 60 * 60 * 1000, 65.8)]),
  ("monkey", [("no2", 92 * 60 * 60 * 1000, 65.8)]),
  ("goat", [("o3", 2 * 60 * 60 * 1000, 29.4)]),
  ("cat or dog", [("o3", 2 * 60 * 60 * 1000, 29.4)]),
  ("mouse", [("o3", 3 * 60 * 60 * 1000, 41), ("so2", 3 * 60 * 60 * 1000, 561)]),
  ("rat", [("o3", 3 * 60 * 60 * 1000, 43)]),
  ("rabbit", [("o3", 3 * 60 * 60 * 1000, 71), ("so2", 7 * 60 * 60 * 1000, 714)]),
  ("chick", [("co", 60 * 60 * 1000, 9.162)])]

rates :: PM -> Float
rates PM10 = 0.008
rates PM25 = 0.007

deathRateDelta:: Float -> PM -> Float
deathRateDelta ng pm = ng * (rates pm)

-- makeAnimalRaw :: String -> [(T.Text, Int, Float)] -> Animal
-- makeAnimalLife :: T.Text -> Int -> Float -> AnimalLife



populateNewSource :: T.Text -> Float -> Int -> AnimalLife
populateNewSource m c count = AL { measure = m, lifeexp = 0, concentration = c * (fromIntegral count)}

populateNewSourceDelta :: T.Text -> Float -> AnimalLife
populateNewSourceDelta m delta = AL { measure = m, lifeexp = 0, concentration = delta}

mergeALsConc :: AnimalLife -> AnimalLife -> AnimalLife
mergeALsConc (AL {measure = m0, lifeexp = le0, concentration = c0}) (AL {measure = m1, lifeexp = le1, concentration = c1}) = AL {measure = m0, lifeexp = le0, concentration = c0 + c1}

populateNewSourceFull :: T.Text -> Float -> Float -> Int -> AnimalLife
populateNewSourceFull m baseConc delta c = mergeALsConc (populateNewSource m baseConc c) (populateNewSourceDelta m delta)



generateData :: Int -> Float -> Car -> [(String, Float)] -> [Animal] -> [Animal]
generateData rate eps car currentState base = map update base
  where update = (\a -> A {  animal = (animal a), info = newAttrs } )
        newAttrs :: [AnimalLife]
        newAttrs = undefined

-- animals =
