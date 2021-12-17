module AoC2021.Day17 where

import           Text.ParserCombinators.ReadP
import           Data.Char
import           Data.List

inputFile = readFile "src/AoC2021/Day17.txt"

parseNumber :: ReadP Int
parseNumber = do
  sign   <- option ' ' (char '-')
  digits <- munch isDigit
  return $ read (sign : digits)

parse = do
  string "target area: x="
  x0 <- parseNumber
  string ".."
  x1 <- parseNumber
  string ", y="
  y0 <- parseNumber
  string ".."
  y1 <- parseNumber

  return ((x0, x1), (y0, y1))

step :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
step (v@(vx, vy), p@(px, py)) = ((max 0 $ vx - 1, vy - 1), (px + vx, py + vy))

missed y0 (_, (_, py)) = py < y0

test ((x0, x1), (y0, y1)) v =
  any (\(_, (x, y)) -> x0 <= x && x <= x1 && y0 <= y && y <= y1)
    . takeWhile (not . missed y0)
    . iterate step
    $ (v, (0, 0))

main :: IO ()
main = do
  target@((x0, x1), (y0, y1)) <- fst . last . readP_to_S parse <$> inputFile

  let v             = pred . negate $ y0
  let part1         = v * (v - 1) `div` 2

  let allVelocities = (,) <$> [0 .. x1] <*> [y0 .. negate y0]
  let part2         = length . filter (test target) $ allVelocities

  putStr "part 1: " >> print part1
  putStr "part 2: " >> print part2
