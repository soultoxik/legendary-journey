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

type Stations = [StationInfo]

instance ToJSON Entry
instance ToJSON Coord
instance ToJSON Measurement
instance ToJSON StationInfo
