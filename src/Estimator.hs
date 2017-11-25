{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Estimator where
import Types

animalsBase :: [Animal]
animalsBase = map (\(n, vals) -> makeAnimalRaw n vals) [
  ("chicken", [("co", "60 min", 4.581)]),
  ("pigeon", [("co", "60 min", 9.162)]),
  ("pig", [("co", "60 min", 9.162), ("o3", "3 hour", 102), ("so2", "2.75 hour", 89)]),
  ("any living thing", [("no2", "ASAP", 28.2)]),
  ("mouse", [("no2", "< 92 hours", 6.6)]),
  ("hamster", [("no2", "< 92 hours", 65.8)]),
  ("monkey", [("no2", "< 92 hours", 65.8)]),
  ("goat", [("o3", "2 hour", 29.4)]),
  ("cat or dog", [("o3", "2 hour", 29.4)]),
  ("mouse", [("o3", "3 hour", 41), ("so2", "3.5 hour", 561)]),
  ("rat", [("o3", "3 hour", 43)]),
  ("rabbit", [("o3", "3 hour", 71), ("so", "7 hour", 714)]),
  ("chick", [("co", "60 min", 9.162)])]

rates :: PM -> Float
rates PM10 = 0.008
rates PM25 = 0.007

deathRateDelta:: Float -> PM -> Float
deathRateDelta ng pm = ng * (rates pm)


-- animals =
