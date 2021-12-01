module AoC2021.Day01 where

import           Control.Monad
import           Data.List

inputFile = readFile "src/AoC2021/Day01.txt"

main :: IO ()
main = do
  parsedInput <- map read . lines <$> inputFile

  let part1 =
        length . filter (> 0) . (zipWith (-) <$> tail <*> id) $ parsedInput
  putStr "part 1: " >> print part1

  let part2 =
        length
          . filter (> 0)
          . (zipWith (-) <$> tail . tail . tail <*> id)
          $ parsedInput
  putStr "part 2: " >> print part2

