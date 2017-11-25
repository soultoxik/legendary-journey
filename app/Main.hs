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
allStations = [
                S {stationId = 0, coord = XY 43.4 11.5, m = M [E "o3" 4.4] },
                S {stationId = 1, coord = XY 3.42 45.4, m = M [E "o3" 3.4] },
                S {stationId = 2, coord = XY 0.5 51.7, m = M [E "o3" 4.2] },
                S {stationId = 3, coord = XY 44.4 51.5, m = M [E "o3" 4.6] },
                S {stationId = 4, coord = XY 12.3 44.5, m = M [E "o3" 5.8] },
                S {stationId = 5, coord = XY 53.4 5.5, m = M [E "o3" 4.gi5] }
                ]

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
