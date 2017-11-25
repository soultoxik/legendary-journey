{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Estimator where
import Types


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

generateData :: Float -> Float -> Car -> [Animal] -> [Animal]
generateData rate eps car ans = undefined


-- animals =
