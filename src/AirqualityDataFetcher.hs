{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AirqualityDataFetcher
    ( getStationNO2Level
    ) where

import Data.Monoid
import Data.Aeson
import GHC.Generics
import Data.Text
import Network.HTTP.Conduit

type StationId = Int

apiUrl :: String
apiUrl = "http://biomi.kapsi.fi/tools/airquality/"

requestBuilder :: StationId -> String -> String
requestBuilder sid meas = apiUrl <> "?p=" <> meas <> "&ss=" <> (show sid) 


getStationData :: StationId -> String -> IO (Maybe Value)
getStationData sid meas = do
    rawJson <- simpleHttp $ requestBuilder sid meas
    return (decode rawJson :: Maybe Value)

getStationNO2LevelJson :: Int -> IO String
getStationNO2LevelJson stationId = do
    response <- getStationData stationId "nitrogendioxide"
    case response of
                    (Just v) -> return (show $ v)
                    Nothing -> return ""

data ErrorMsg =
     ErrorMsg { error :: Bool
               ,message :: !Text
              } deriving (Show, Generic)

instance FromJSON ErrorMsg

parseNO2Level :: IO String -> IO String
parseNO2Level json = do
                    str <- json
                    return str

getStationNO2Level :: Int -> IO String
getStationNO2Level stationId = do
    response <- getStationNO2LevelJson stationId
    return response
