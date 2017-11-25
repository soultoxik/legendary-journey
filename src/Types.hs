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

data Stations = St { stations :: [StationInfo] } deriving (Show, Generic)

makeAnimal :: String -> [AnimalLife] -> Animal
makeAnimal name infos = A {animal = name, info = infos}

makeAnimalLife :: T.Text -> T.Text -> Float -> AnimalLife
makeAnimalLife measure lifeexp concentration = AL { measure = measure, lifeexp = lifeexp, concentration = concentration }

makeAnimalRaw :: String -> [(T.Text, T.Text, Float)] -> Animal
makeAnimalRaw name vals = makeAnimal name $ map (\(x, y, z) -> makeAnimalLife x y z) vals

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
