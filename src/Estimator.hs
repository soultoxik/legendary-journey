{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Estimator where
import Types
import qualified Data.Text as T
import qualified Data.String as DS
import qualified Data.List as DL



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

-- baseConc from car
populateNewSourceFull :: T.Text -> Float -> Float -> Int -> AnimalLife
populateNewSourceFull m carConc delta c = mergeALsConc (populateNewSource m carConc c) (populateNewSourceDelta m delta)

populateBase :: T.Text -> Float -> AnimalLife
populateBase m baseConc = populateNewSource m baseConc 1

populateFull :: T.Text -> Float -> Float -> Float -> Int -> AnimalLife
populateFull measure baseConc carConc delta count = mergeALsConc (populateNewSourceFull measure carConc delta count) (populateBase measure baseConc)

lifeExpByKey :: Animal -> T.Text -> Int
lifeExpByKey (A {info = infos}) key =
  let res = DL.find (\ai -> (measure ai) == key) infos in
              case res of Just (AL {lifeexp = lifeexp}) -> lifeexp
                          Nothing -> 0

concentrationByKey :: Animal -> T.Text -> Float
concentrationByKey (A {info = infos}) key =
  let res = DL.find (\ai -> ( measure ai) == key) infos in
              case res of Just (AL {concentration = concentration}) -> concentration
                          Nothing -> 0.0

newLifeexp :: String -> Float -> Animal -> Int -> Float -> Int
newLifeexp m baseConc a rate eps= (lifeExpByKey a (DS.fromString m)) * round ((concentrationByKey a (DS.fromString m)) / (newConc m baseConc a rate eps))

newConc :: String -> Float -> Animal -> Int -> Float -> Float
newConc m baseConc a rate eps = baseConc + (carConc' (DS.fromString m) car) * (fromIntegral rate) +  eps

carConc' :: String -> Car -> Float
carConc' measure car | measure == "co" = getCO car
                    | measure == "o3" = getO3 car
                    | measure == "so2" = getSO2 car
                    | measure == "no2" = getNO2 car
                    | otherwise = 0.0

generateData :: Int -> Float -> Car -> [(String, Float)] -> [Animal] -> [Animal]
generateData rate eps car currentState base = map update base
  where update a =  A {  animal = (animal a), info = (newAttrs a)}
        newAttrs :: Animal -> [AnimalLife]
        newAttrs a = map (\s -> magic s a) currentState
        magic :: (String, Float) ->Animal-> AnimalLife
        magic (m, baseConc) a = AL { measure = ( DS.fromString m), lifeexp = (newLifeexp m baseConc a rate eps), concentration = (newConc m baseConc a rate eps)}


-- animals =
