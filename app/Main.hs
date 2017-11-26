{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Web.Scotty
import Network.Wai.Handler.Warp
import Types
import AirqualityDataFetcher
import qualified Data.String as DS
import Estimator
import Data.Time.Clock.POSIX
import qualified Estimator as Est
import System.Random



allStations :: Stations
allStations = St { stations = [
                S {stationId = 0, coord = XY 43.4 11.5, m = M [E "o3" 4.4] },
                S {stationId = 1, coord = XY 3.42 45.4, m = M [E "o3" 3.4] },
                S {stationId = 2, coord = XY 0.5 51.7, m = M [E "o3" 4.2] },
                S {stationId = 3, coord = XY 44.4 51.5, m = M [E "o3" 4.6] },
                S {stationId = 4, coord = XY 12.3 44.5, m = M [E "o3" 5.8] },
                S {stationId = 5, coord = XY 53.4 5.5, m = M [E "o3" 4.4] }
                ]
              }

stationInfo :: [Animal]
stationInfo = [
                (A "dog" [
                  (AL {
                        measure = "o3",
                        lifeexp = 3333,
                        concentration = 0.33
                        }
                      )
                  ]
                ) ,
                (A "pes" [
                  (AL {
                        measure = "no2",
                        lifeexp = 666,
                        concentration = 14.33
                        }
                      )
                  ]
                )
              ]

byId :: Int -> StationInfo -> Bool
byId i (S {stationId = ci}) = (i == ci)


main = do
  putStrLn "Starting Server..."
  let settings = setPort 3000 $ setHost "0.0.0.0" defaultSettings
  scottyOpts (Options {verbose = 1,  settings = settings}) $ do
    get "/stations" $ do
      addHeader "Access-Control-Allow-Origin" "*"
      json allStations

    get "/station/:id" $ do
      pid <- param "id"
      addHeader "Access-Control-Allow-Origin" "*"
      json $ filter (byId pid) $ stations allStations

    get "/info/:id" $ do
      addHeader "Access-Control-Allow-Origin" "*"

      -- pid <- param "id"
      -- generateData :: Int -> Float -> Car -> [(String, Float)] -> [Animal] -> [Animal]
      json $ stationInfo
    get "/fullinfo/:lat/:lon" $ do
      seed <- liftAndCatchIO $ newStdGen
      addHeader "Access-Control-Allow-Origin" "*"
      -- count <- liftAndCatchIO $ (randomIO :: IO Float)
      let (eps, seed0) =  randomR (0.0 :: Float, 1.0 :: Float) seed
      let (rate, _) =  randomR (1 :: Int, 5 :: Int) seed0

      bases <- liftAndCatchIO $ infoById 564

      let stationInfoAct = Est.generateData rate eps car bases Est.animalsBase
      json $ FI {animals =  stationInfoAct, sights = ["Karnaval", "Kipelov", "Ygaraem tyt"], topPolluted = 3, timeToBusStop = 12}
                        -- where filterZero = map clearZeroALs
                        --       clearZeroALs (A {animal = an, info=ifs}) = A {animal = an, info = filter filterZero ifs}
                        --       filterZero :: AnimalLife -> Bool
                        --       filterZero AL {lifeExp = le} | le == 0    = True
                        --                                    | otherwise  = False

    get "/pm/:lat/:lon" $ do
      addHeader "Access-Control-Allow-Origin" "*"
      -- lat <- param "lat"
      -- lon <- param "lon"
      let pm10 = deathRateDelta 200 PM10
          pm25 = deathRateDelta 200 PM25
      json $ PMP {pm10 = pm10, pm25 = pm25}
