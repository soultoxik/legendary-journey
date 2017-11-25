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


getRawStationJsonData :: StationId -> String -> IO B.ByteString
getRawStationJsonData sid meas = do
    rawJson <- simpleHttp $ requestBuilder sid meas
    return rawJson 

handleErrorMsg :: B.ByteString -> String
handleErrorMsg json = case errorResult of
                                          Right msg -> show $ message msg
                                          Left msg -> show msg
                      where errorResult  = (eitherDecode json) :: Either String ErrorMsg


getStationNO2LevelJson :: Int -> IO (Either String Int)
getStationNO2LevelJson stationId = do
    json <- getRawStationJsonData stationId "nitrogendioxide"
    let response = eitherDecode json :: Either String NO2Data
    case response of
                    Right msg -> return (Right $ time (latest msg) )
                    Left err -> return (Left $ handleErrorMsg json)

getStationNO2Level :: Int -> IO (Maybe Float)
getStationNO2Level stationId = do
    response <- getStationNO2LevelJson stationId
    case response of
                    Right t -> return (Just $ fromIntegral t)
                    Left msg -> return Nothing
