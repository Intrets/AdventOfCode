module Main where

import qualified AoC2020
import qualified AoC2021

import           System.Environment
import           Text.Read
import           Data.Maybe

readInt :: String -> Maybe Int
readInt = readMaybe

year :: Int -> Maybe (IO ())
year 2020 = Just $ putStrLn "Year 2020:" >> AoC2020.main
year 2021 = Just $ putStrLn "Year 2021:" >> AoC2021.main
year _ = Nothing

main :: IO ()
main = do
  years <- mapMaybe year . mapMaybe readInt <$> getArgs
  if null years then sequence_ $ mapMaybe year [2020, 2021] else sequence_ years

