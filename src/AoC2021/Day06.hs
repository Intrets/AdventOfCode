module AoC2021.Day06 where

import           Data.List.Split
import           Data.List
import           Control.Arrow
import           Data.Bool
import           Data.Function

inputFile = readFile "src/AoC2021/Day06.txt"

step (count, 0  ) = [(count, 6), (count, 8)]
step (count, age) = [(count, age - 1)]

main :: IO ()
main = do
  fishes <-
    map ((,) <$> length <*> head)
    .   group
    .   sort
    .   map read
    .   splitOn ","
    <$> inputFile

  let answer day =
        sum
          . map fst
          . (!! day)
          . iterate
              ( map (foldl1 (first . (+) . fst))
              . groupBy ((==) `on` snd)
              . sortOn snd
              . concatMap step
              )
          $ fishes

  putStr "part 1: " >> print (answer 80)
  putStr "part 2: " >> print (answer 256)

