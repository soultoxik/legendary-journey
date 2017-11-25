{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.String
import           Data.Aeson
import           GHC.Generics
import qualified Data.Text as T

data Coord = XY Float Float deriving (Generic, Show)
-- data Measurement = M {
--   co :: Maybe Float,
--   no2 :: Maybe Float,
--   trs :: Maybe Float,
--   o3 :: Maybe Float,
--   pm10 :: Maybe Float,
--   pm25 :: Maybe Float,
--   so2 :: Maybe Float
-- }
data Entry = E T.Text Float deriving (Generic, Show)
data Measurement = M [Entry] deriving (Generic, Show)
data StationInfo = S
  { stationId   :: Int
  , coord :: Coord
  , m :: Measurement
  } deriving (Generic, Show)

data Stations = St { stations :: [StationInfo] } deriving (Show, Generic)

makeAnimal :: String -> [AnimalLife] -> Animal
makeAnimal name infos = A {animal = name, info = infos}

makeAnimalLife :: T.Text -> Int -> Float -> AnimalLife
makeAnimalLife measure lifeexp concentration = AL { measure = measure, lifeexp = lifeexp, concentration = concentration }

makeAnimalRaw :: String -> [(T.Text, Int, Float)] -> Animal
makeAnimalRaw name vals = makeAnimal name $ map (\(x, y, z) -> makeAnimalLife x y z) vals

data Animal = A {
  animal :: String,
  info :: AInfo
} deriving (Show, Generic)

type AInfo = [AnimalLife]
data AnimalLife = AL {
  measure :: T.Text,
  lifeexp :: Int,
  concentration :: Float
} deriving (Show, Generic)


instance ToJSON Animal
instance ToJSON AnimalLife


instance ToJSON Stations
instance ToJSON Entry
instance ToJSON Coord
instance ToJSON Measurement
instance ToJSON StationInfo
