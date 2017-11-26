{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module AirqualityDataFetcher
    (
       infoById
    ) where

import Data.Monoid
import Data.Aeson
import GHC.Generics
import Data.Text
import qualified Data.ByteString.Lazy as B
import qualified ParserTypes as PT
import Network.HTTP.Conduit

type StationId = Int

apiUrl :: String
apiUrl = "http://biomi.kapsi.fi/tools/airquality/"

requestBuilder :: StationId -> String -> String
requestBuilder sid meas = apiUrl <> "?p=" <> meas <> "&ss=" <> (show sid) 


getStationRawJsonData :: StationId -> String -> IO B.ByteString
getStationRawJsonData sid meas = do
    rawJson <- simpleHttp $ requestBuilder sid meas
    return rawJson 

handleErrorMsg :: B.ByteString -> String
handleErrorMsg json = case errorResult of
                                          Right msg -> show $ PT.message msg
                                          Left msg -> show msg
                      where errorResult  = (eitherDecode json) :: Either String PT.ErrorMsg


getStationMeasData :: FromJSON a => Int -> String -> IO (Either String a)
getStationMeasData stationId meas = do
    json <- getStationRawJsonData stationId meas
    let response = eitherDecode json :: FromJSON a => Either String a
    case response of
                    Right msg -> return (Right $ msg )
                    Left err -> return (Left $ handleErrorMsg json)

getStationNO2LevelData :: Int -> IO (Either String Float)
getStationNO2LevelData stationId = do
        meas <- getStationMeasData stationId "nitrogendioxide" :: IO (Either String PT.MeasData)
        case meas of
                    Right m -> return (Right $ PT.value $ PT.latest m)
                    Left m -> return (Left $ show m)

getStationCOLevelData :: Int -> IO (Either String Float)
getStationCOLevelData stationId = do
        meas <- getStationMeasData stationId "carbonmonoxide" :: IO (Either String PT.MeasData)
        case meas of
                    Right m -> return (Right $ PT.value $ PT.latest m)
                    Left m -> return (Left $ show m)


getStationNO2Level :: Int -> IO (Maybe Float)
getStationNO2Level stationId = do
    response <- getStationNO2LevelData stationId
    case response of
                    Right t -> return (Just $ t)
                    Left msg -> return Nothing

getStationCOLevel :: Int -> IO (Maybe Float)
getStationCOLevel stationId = do
    response <- getStationCOLevelData stationId
    case response of
                    Right t -> return (Just $ t)
                    Left msg -> return Nothing

getStationSO2LevelData :: Int -> IO (Either String Float)
getStationSO2LevelData stationId = do
        meas <- getStationMeasData stationId "sulphurdioxide" :: IO (Either String PT.MeasData)
        case meas of
                    Right m -> return (Right $ PT.value $ PT.latest m)
                    Left m -> return (Left $ show m)

getStationSO2Level :: Int -> IO (Maybe Float)
getStationSO2Level stationId = do
    response <- getStationSO2LevelData stationId
    case response of
                    Right t -> return (Just $ t)
                    Left msg -> return Nothing

getStationO3LevelData :: Int -> IO (Either String Float)
getStationO3LevelData stationId = do
        meas <- getStationMeasData stationId "ozone" :: IO (Either String PT.MeasData)
        case meas of
                    Right m -> return (Right $ PT.value $ PT.latest m)
                    Left m -> return (Left $ show m)

getStationO3Level :: Int -> IO (Maybe Float)
getStationO3Level stationId = do
    response <- getStationO3LevelData stationId
    case response of
                    Right t -> return (Just $ t)
                    Left msg -> return Nothing

type Measurement = (String, Maybe Float)
type Measurements = [Measurement]

infoById :: Int -> IO Measurements
infoById stationId = do
    let toxins = ["no2", "so2", "co", "o3"]
    let getters = Prelude.map ($ stationId) [getStationNO2Level,  getStationSO2Level, getStationCOLevel, getStationO3Level]
    result0 <- (getters !! 0)
    result1 <- (getters !! 1)
    result2 <- (getters !! 2)
    result3 <- (getters !! 3)
    let results = Prelude.zip toxins [result0, result1, result2, result3]
    return results
