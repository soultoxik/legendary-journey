{-# LANGUAGE OverloadedStrings #-}

module AirqualityDataFetcher
    ( fetchSomeData
    ) where

import Data.Monoid
import Data.Aeson
import Network.HTTP.Conduit

type NO2Id = String

apiUrl :: String
apiUrl = "http://biomi.kapsi.fi/tools/airquality/?p=nitrogendioxide&ss="

requestUrl :: String
requestUrl = ""

requestBuilder :: NO2Id -> String
requestBuilder vid = apiUrl <> vid <> requestUrl


getVenue :: NO2Id -> IO (Maybe Value)
getVenue vid = do
    rawJson <- simpleHttp $ requestBuilder vid
    return (decode rawJson :: Maybe Value)

fetchSomeData :: IO String
fetchSomeData = do
    response <- getVenue "563"
    case response of
                    (Just v) -> return (show $ v)
                    Nothing -> return ""
