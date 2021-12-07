module AoC2021.Day06 where

import           Data.List.Split
import           Data.List
import           Control.Arrow
import           Data.Bool
import           Data.Function
import           Control.Monad

inputFile = readFile "src/AoC2021/Day06.txt"

step (count, 0  ) = [(count, 6), (count, 8)]
step (count, age) = [(count, age - 1)]


main :: IO ()
main = do
  fishes <-
    map ((,) <$> fromIntegral . length <*> head)
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

  fishes2 <-
    map (pred . length)
    .   group
    .   sort
    .   (++) (enumFromTo 0 6)
    .   map read
    .   splitOn ","
    <$> inputFile

  let answer2 day =
        sum
          . concat
          . (!! day)
          . iterate
              (ap ((map tail .) . flip (map . flip (++) . pure))
                  (sum . head . transpose)
              )
          . (: [replicate 9 0])
          $ fishes2

  putStr "part 1: " >> print (answer2 80)
  putStr "part 2: " >> print (answer2 256)



