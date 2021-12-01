module AoC2021.Day01 where

import           Control.Monad
import           Data.List

inputFile = readFile "src/AoC2021/Day01.txt"

readInt = read :: String -> Int

main :: IO ()
main = do
  parsedInput <- map readInt . lines <$> inputFile

  let part1 =
        length . filter (> 0) . (zipWith (-) <$> tail <*> id) $ parsedInput
  putStr "part 1: " >> print part1

  paddedInput <- map readInt . lines <$> inputFile
  let part2 =
        length
          . filter (> 0)
          . (zipWith (-) <$> tail . tail . tail <*> id)
          $ parsedInput
  putStr "part 2: " >> print part2

