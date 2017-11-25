{-# LANGUAGE OverloadedStrings #-}

module AirqualityDataFetcher
    ( getStationNO2Level
    ) where

import Data.Monoid
import Data.Aeson
import Network.HTTP.Conduit

type NO2Id = Int

apiUrl :: String
apiUrl = "http://biomi.kapsi.fi/tools/airquality/?p=nitrogendioxide&ss="

requestBuilder :: NO2Id -> String
requestBuilder sid = apiUrl <> (show sid)


getStationData :: NO2Id -> IO (Maybe Value)
getStationData sid = do
    rawJson <- simpleHttp $ requestBuilder sid
    return (decode rawJson :: Maybe Value)

getStationNO2Level :: Int -> IO String
getStationNO2Level stationId = do
    response <- getStationData stationId
    case response of
                    (Just v) -> return (show $ v)
                    Nothing -> return ""
