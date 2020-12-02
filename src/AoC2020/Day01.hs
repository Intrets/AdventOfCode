module AoC2020.Day01 where

import           Control.Monad

solution :: Int -> IO ()
solution n =
  print
    =<< product
    .   head
    .   filter ((== 2020) . sum)
    .   replicateM n
    .   map read
    .   words
    <$> readFile "src/AoC2020/Day01.txt"

main :: IO ()
main = do
  putStr "part one: " >> solution 2
  putStr "part two: " >> solution 3

