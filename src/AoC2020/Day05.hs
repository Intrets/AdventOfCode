module AoC2020.Day05 where

import           Data.List
import           Data.Bits
import           Control.Applicative
import           Data.Function
import           Control.Monad

readBin :: String -> Int
readBin =
  sum
    . zipWith (*) (iterate (`shiftL` 1) 1)
    . map (length . intersect "BR" . pure)
    . reverse

main :: IO ()
main = do
  inputData <- map readBin . lines <$> readFile "src/AoC2020/Day05.txt"

  putStr "part one: " >> print (maximum inputData)
  putStr "part two: " >> print
    (foldl1 xor (ap (++) (liftA2 enumFromTo minimum maximum) inputData))

