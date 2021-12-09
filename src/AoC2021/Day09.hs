module AoC2021.Day09 where

import           Data.List
import           Data.List.Split
import qualified Data.Map                      as M
import           Control.Monad.State
import           Control.Arrow

inputFile = readFile "src/AoC2021/Day09.txt"

floodFill
  :: ((Int, Int) -> Int) -> (Int, Int) -> State (M.Map (Int, Int) Bool) Int
floodFill getHeight point = do
  visited <- gets (M.! point)
  if (getHeight point == 9) || visited
    then return 0
    else do
      modify (M.insert point True)
      succ . sum <$> mapM (floodFill getHeight . ($point))
                          [first succ, first pred, second succ, second pred]

getBasinSizes
  :: [(Int, Int)] -> ((Int, Int) -> Int) -> State (M.Map (Int, Int) Bool) [Int]
getBasinSizes points getHeight =
  filter (> 0) <$> mapM (floodFill getHeight) points

main :: IO ()
main = do
  part1 <- map (map ((read :: String -> Int) . pure)) . lines <$> inputFile

  let edge      = 9

  let padded1 = map ((++ [edge]) . ([edge] ++)) part1
  let height    = length . head $ padded1
  let padded2 = [replicate height edge] ++ padded1 ++ [replicate height edge]
  let width     = length padded2

  let flattened = concat padded2
  let neighbours = zipWith5 (\a b c d e -> (a, [b, c, d, e]))
                            flattened
                            (tail flattened)
                            (edge : flattened)
                            (drop height flattened)
                            (replicate height edge ++ flattened)
  let part1 =
        sum . map (succ . fst) . filter (uncurry (all . (<))) $ neighbours

  let points    = (,) <$> [0 .. width - 1] <*> [0 .. height - 1]
  let heightMap = zip points flattened
  let getHeight = (M.!) (M.fromList heightMap)
  let visited   = M.fromList $ zip points (repeat False)
  let part2 = product . take 3 . sortOn negate $ evalState
        (getBasinSizes points getHeight)
        visited

  putStr "part 1: " >> print part1
  putStr "part 2: " >> print part2

