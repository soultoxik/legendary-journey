{-# LANGUAGE OverloadedStrings #-}

module Estimator where
import Types

animalsBase :: [Animal]
animalsBase = map (\(n, vals) -> makeAnimalRaw n vals) [
  ("chicken", [("co", "60 min", 4.581)]),
  ("pigeon", [("co", "60 min", 9.162)]),
  ("pig", [("co", "60 min", 9.162)]),
  ("any living thing", [("no2", "?? min", 28.2)]),
  ("mouse ", [("no2", "< 92 hours", 6.6)]),
  ("hamster ", [("no2", "< 92 hours", 65.8)]),
  ("monkey ", [("no2", "< 92 hours", 65.8)]),

  ("chick", [("co", "60 min", 9.162)])]

-- animals =
