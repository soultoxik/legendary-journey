{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.String
import           Data.Aeson
import           GHC.Generics
import qualified Data.Text as T

data PM = PM10 | PM25 deriving (Show, Generic)
data PMPair = PMP {pm10 :: Float, pm25 :: Float } deriving (Show, Generic)

data Coord = XY Float Float deriving (Generic, Show)
data Entry = E T.Text Float deriving (Generic, Show)
data Measurement = M [Entry] deriving (Generic, Show)
data StationInfo = S
  { stationId   :: Int
  , coord :: Coord
  , m :: Measurement
  } deriving (Generic, Show)


grammsToConcentration :: Float -> Float
grammsToConcentration g = (g / 1500.0) * 1000000.0

data Stations = St { stations :: [StationInfo] } deriving (Show, Generic)

makeAnimal :: String -> [AnimalLife] -> Animal
makeAnimal name infos = A {animal = name, info = infos}

makeAnimalLife :: T.Text -> T.Text -> Float -> AnimalLife
makeAnimalLife measure lifeexp concentration = AL { measure = measure, lifeexp = lifeexp, concentration = concentration }

makeAnimalRaw :: String -> [(T.Text, T.Text, Float)] -> Animal
makeAnimalRaw name vals = makeAnimal name $ map (\(x, y, z) -> makeAnimalLife x y z) vals

-- co no2 so2 o3
type CO = Float
type NO2 = Float
type SO2 = Float
type O3 = Float
type PM10F = Float
type PM25F = Float

data Car = Car CO NO2 SO2 O3 PM10F PM25F
car :: Car
car = Car (grammsToConcentration 8.73) (grammsToConcentration 1.5) (grammsToConcentration 0.6) (grammsToConcentration 0.0) (grammsToConcentration 0.06) (grammsToConcentration 0.02)



data Animal = A {
  animal :: String,
  info :: AInfo
} deriving (Show, Generic)

type AInfo = [AnimalLife]
data AnimalLife = AL {
  measure :: T.Text,
  lifeexp :: T.Text,
  concentration :: Float
} deriving (Show, Generic)


instance ToJSON Animal
instance ToJSON AnimalLife

instance ToJSON Stations
instance ToJSON Entry
instance ToJSON Coord
instance ToJSON Measurement
instance ToJSON StationInfo

instance ToJSON PM
instance ToJSON PMPair
