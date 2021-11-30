module AoC2020 where

import           AoC2020.Day01
import           AoC2020.Day02
import           AoC2020.Day03
import           AoC2020.Day04
import           AoC2020.Day05
import           AoC2020.Day06
import           AoC2020.Day07
import           AoC2020.Day08
import           AoC2020.Day09
import           AoC2020.Day10
import           AoC2020.Day11
import           AoC2020.Day12
import           AoC2020.Day13
import           AoC2020.Day14
import           AoC2020.Day15
import           AoC2020.Day16
import           AoC2020.Day17
import           AoC2020.Day18
import           AoC2020.Day19
import           AoC2020.Day20
import           AoC2020.Day21
import           AoC2020.Day22
import           AoC2020.Day23
import           AoC2020.Day24
import           AoC2020.Day25

import           Text.Read
import           Data.Maybe
import           Control.Monad

import           System.Environment

day n = putStr "day " >> print n >> day' n
day' 1  = AoC2020.Day01.main
day' 2  = AoC2020.Day02.main
day' 3  = AoC2020.Day03.main
day' 4  = AoC2020.Day04.main
day' 5  = AoC2020.Day05.main
day' 6  = AoC2020.Day06.main
day' 7  = AoC2020.Day07.main
day' 8  = AoC2020.Day08.main
day' 9  = AoC2020.Day09.main
day' 10 = AoC2020.Day10.main
day' 11 = AoC2020.Day11.main
day' 12 = AoC2020.Day12.main
day' 13 = AoC2020.Day13.main
day' 14 = AoC2020.Day14.main
day' 15 = AoC2020.Day15.main
day' 16 = AoC2020.Day16.main
day' 17 = AoC2020.Day17.main
day' 18 = AoC2020.Day18.main
day' 19 = AoC2020.Day19.main
day' 20 = AoC2020.Day20.main
day' 21 = AoC2020.Day21.main
day' 22 = AoC2020.Day22.main
day' 23 = AoC2020.Day23.main
day' 24 = AoC2020.Day24.main
day' 25 = AoC2020.Day25.main

readInt :: String -> Maybe Int
readInt = readMaybe

main :: IO ()
main = do
  days <- filter ((&&) <$> (> 0) <*> (< 26)) . mapMaybe readInt <$> getArgs

  if null days then mapM_ day [1 .. 25] else mapM_ day days

