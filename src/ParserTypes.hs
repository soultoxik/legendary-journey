{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ParserTypes where

import GHC.Generics
import Data.Text
import Data.Aeson

data MeasurementInfo =
    MeasurementInfo {
--            data :: Float,
            time :: Int
--            EN :: Text
    } deriving (Show, Generic)


data NO2Data =
     NO2Data { error :: Bool
               ,latest :: MeasurementInfo
              } deriving (Show, Generic)

data ErrorMsg =
     ErrorMsg { error :: Bool
               ,message :: !Text
              } deriving (Show, Generic)

instance FromJSON ErrorMsg
instance FromJSON MeasurementInfo
instance FromJSON NO2Data

