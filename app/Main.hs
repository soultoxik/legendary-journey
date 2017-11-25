{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Web.Scotty
import Network.Wai.Handler.Warp
import Types




allStations :: Stations
allStations = [S {stationId = 0, coord = XY 43.4 54.5, m = M [E "o3" 4.4] }]

byId :: Int -> StationInfo -> Bool
byId i (S {stationId = ci}) = (i == ci)

main = do
  putStrLn "Starting Server..."
  let settings = setPort 3000 $ setHost "10.100.41.153" defaultSettings
  scottyOpts (Options {verbose = 1,  settings = settings}) $ do
    get "/stations" $ do
      json allStations

    get "/station/:id" $ do
      pid <- param "id"
      json $ filter (byId pid) allStations
