{-# LANGUAGE ViewPatterns #-}
module AoC2021.Day02 where

inputFile = readFile "src/AoC2021/Day02.txt"

part1 (x, y) ["forward", read -> value] = (x + value, y)
part1 (x, y) ["up"     , read -> value] = (x, y - value)
part1 (x, y) ["down"   , read -> value] = (x, y + value)

part2 (x, y, aim) ["forward", read -> value] =
  (x + value, y + aim * value, aim)
part2 (x, y, aim) ["up"  , read -> value] = (x, y, aim - value)
part2 (x, y, aim) ["down", read -> value] = (x, y, aim + value)

main :: IO ()
main = do
  part1Answer <-
    uncurry (*) . foldl part1 (0, 0) . map words . lines <$> inputFile

  part2Answer <-
    (\(x, y, _) -> x * y)
    .   foldl part2 (0, 0, 0)
    .   map words
    .   lines
    <$> inputFile

  putStr "part 1: " >> print part1Answer
  putStr "part 2: " >> print part2Answer

