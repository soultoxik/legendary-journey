{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ParserTypes where

import GHC.Generics
import Data.Text
import Data.Aeson
import Control.Monad

data MeasurementInfo =
    MeasurementInfo {
            value :: Float,
            time :: Int,
            status :: Text
    } deriving (Show)

instance FromJSON MeasurementInfo where
 parseJSON (Object v) =
    MeasurementInfo <$> v .: "data"
                    <*> v .: "time"
                    <*> v .: "EN"
 parseJSON _ = mzero

data NO2Data =
     NO2Data { error :: Bool
               ,latest :: MeasurementInfo
              } deriving (Show, Generic)

data ErrorMsg =
     ErrorMsg { error :: Bool
               ,message :: !Text
              } deriving (Show, Generic)

instance FromJSON ErrorMsg
instance FromJSON NO2Data

