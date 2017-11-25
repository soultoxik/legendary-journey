{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module AirqualityDataFetcher
    ( getStationNO2Level
    ) where

import Data.Monoid
import Data.Aeson
import GHC.Generics
import Data.Text
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit

type StationId = Int

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

apiUrl :: String
apiUrl = "http://biomi.kapsi.fi/tools/airquality/"

requestBuilder :: StationId -> String -> String
requestBuilder sid meas = apiUrl <> "?p=" <> meas <> "&ss=" <> (show sid) 


getStationData :: StationId -> String -> IO B.ByteString
getStationData sid meas = do
    rawJson <- simpleHttp $ requestBuilder sid meas
    return rawJson 

getStationNO2LevelJson :: Int -> IO String
getStationNO2LevelJson stationId = do
    response <- (eitherDecode <$> getStationData stationId "nitrogendioxide") :: IO (Either String NO2Data)
    case response of
                    Right msg -> return (show msg)
                    Left err -> return (show err)

parseNO2Level :: IO String -> IO String
parseNO2Level json = do
                    str <- json
                    return str

getStationNO2Level :: Int -> IO String
getStationNO2Level stationId = do
    response <- getStationNO2LevelJson stationId
    return response
