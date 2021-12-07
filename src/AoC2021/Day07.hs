module AoC2021.Day07 where

import           Data.List.Split
import           Data.List
import           Data.Ord
import           Control.Monad

inputFile = readFile "src/AoC2021/Day07.txt"

main :: IO ()
main = do
  crabs <- map read . splitOn "," <$> inputFile

  let part1 =
        minimum
          . (   zipWith ((sum .) . map . ((abs .) . (-)))
            <$> (enumFromTo 0 . maximum)
            <*> repeat
            )

  let
    part2 =
      minimum
        . (   zipWith
              ((sum .) . map . (((`div` 2) .) . (ap (*) succ .) . (abs .) . (-))
              )
          <$> (enumFromTo 0 . maximum)
          <*> repeat
          )

  putStr "part 1: " >> print (part1 crabs)
  putStr "part 2: " >> print (part2 crabs)

