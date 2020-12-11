{-# LANGUAGE LambdaCase #-}

module AoC2020.Day10 where

import           Control.Monad
import           Data.List

solve2 :: Integer -> Integer -> Integer -> [Int] -> Integer
solve2 a b c = \case
  (1 : ds) -> solve2 b c (a + b + c) ds
  (2 : ds) -> solve2 c 0 (b + c) ds
  (3 : ds) -> solve2 0 0 c ds
  []       -> c

main :: IO ()
main = do
  inputData <-
    ap (zipWith (flip (-))) tail
    .   sort
    .   (0 :)
    .   ap (flip (:)) ((+ 3) . maximum)
    .   map read
    .   lines
    <$> readFile "src/AoC2020/Day10.txt"

  putStr "part one: "
  print $ product . map length . group . sort $ inputData

  putStr "part two: "
  print $ solve2 0 0 1 inputData

