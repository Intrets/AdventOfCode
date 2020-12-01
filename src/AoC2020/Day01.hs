module AoC2020.Day01 where

import           Control.Monad

solution n =
  print
    =<< product
    .   head
    .   filter ((== 2020) . sum)
    .   replicateM n
    .   map read
    .   words
    <$> readFile "Day01.txt"

main = do
  putStr "part one: " >> solution 2
  putStr "part two: " >> solution 3

