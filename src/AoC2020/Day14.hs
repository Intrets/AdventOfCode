{-# LANGUAGE ViewPatterns #-}
module AoC2020.Day14 where

import           Data.Either
import           Data.List
import           Data.Bits
import           Control.Arrow
import           Data.Function
import           Control.Monad
import           Control.Applicative

allmasks :: String -> [(Integer, Integer)]
allmasks = f' . zip [ bit i | i <- [0 ..] ] . reverse
 where
  f' :: [(Integer, Char)] -> [(Integer, Integer)]
  f' [] = [(0, 0)]
  f' ((d, c) : rest) =
    let ns = f' rest
    in  case c of
          'X' -> liftA2 (++) (map (second (+ d))) (map (first (+ d))) ns
          _   -> ns

parseLine1 :: [String] -> Either (Integer, Integer) (Integer, Integer)
parseLine1 ["mask", _, mask] = Left
  ( foldl1 ((+) . flip shiftL 1) . map (\c -> if c == '0' then 1 else 0) $ mask
  , foldl1 ((+) . flip shiftL 1) . map (\c -> if c == '1' then 1 else 0) $ mask
  )
parseLine1 [(read . drop 4 . init -> n), _, (read -> mem)] = Right (n, mem)

mask1 :: (Integer, Integer) -> Integer -> Integer
mask1 (zeros, ones) n = (n .|. ones) .&. complement zeros

solve1 :: [Either (Integer, Integer) (Integer, Integer)] -> [(Integer, Integer)]
solve1 ((Left m) : (rights -> mems)) = map (second $ mask1 m) mems

parseLine2
  :: [String] -> Either ([(Integer, Integer)], Integer) (Integer, Integer)
parseLine2 ["mask", _, mask] = Left
  ( allmasks mask
  , foldl1 ((+) . flip shiftL 1) . map (\c -> if c == '1' then 1 else 0) $ mask
  )
parseLine2 [(read . drop 4 . init -> n), _, (read -> mem)] = Right (n, mem)

mask2 :: ([(Integer, Integer)], Integer) -> Integer -> [Integer]
mask2 (masks, ones) n = map (`mask1` (n .|. ones)) masks

solve2
  :: [Either ([(Integer, Integer)], Integer) (Integer, Integer)]
  -> [([Integer], Integer)]
solve2 ((Left m) : (rights -> mems)) = map (first $ mask2 m) mems

main :: IO ()
main = do
  inputData <- lines <$> readFile "src/AoC2020/Day14.txt"

  putStr "part one: "
  let inputData1 =
        groupBy (const isRight) . map (parseLine1 . words) $ inputData

  print
    $ sum
    . map (snd . last)
    . groupBy ((==) `on` fst)
    . sortOn fst
    . concatMap solve1
    $ inputData1

  putStr "part two: "
  let inputData2 =
        groupBy (const isRight) . map (parseLine2 . words) $ inputData

  print
    $ sum
    . map (snd . last)
    . groupBy ((==) `on` fst)
    . sortOn fst
    . concatMap (\(a, b) -> zip a (repeat b))
    . concatMap solve2
    $ inputData2
