{-# LANGUAGE OverloadedStrings #-}

module Estimator where
import Types

animalsBase :: [Animal]
animalsBase = map (\(n, vals) -> makeAnimalRaw n vals) [
  ("dog", [("o3", 1222, 0.33)]) ]

-- animals =
