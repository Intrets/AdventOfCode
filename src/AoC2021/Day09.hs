module AoC2021.Day09 where

import           Data.List
import           Data.List.Split
import qualified Data.Map                      as M
import           Control.Monad.State
import           Control.Arrow

inputFile = readFile "src/AoC2021/Day09.txt"

floodFill :: (Int, Int) -> State (M.Map (Int, Int) Int) Int
floodFill point = do
  s <- gets (M.lookup point)
  case s of
    Nothing -> return 0
    Just 9  -> return 0
    Just _  -> do
      modify (M.delete point)
      succ . sum <$> mapM (floodFill . ($point))
                          [first succ, first pred, second succ, second pred]

getBasinSizes :: [(Int, Int)] -> State (M.Map (Int, Int) Int) [Int]
getBasinSizes points = filter (> 0) <$> mapM floodFill points

main :: IO ()
main = do
  unpadded <- map (map (read . pure)) . lines <$> inputFile

  let edge      = 9

  let padded1 = map ((++ [edge]) . ([edge] ++)) unpadded
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
  let heightMap = M.fromList $ zip points flattened
  let part2 = product . take 3 . sortOn negate $ evalState
        (getBasinSizes points)
        heightMap

  putStr "part 1: " >> print part1
  putStr "part 2: " >> print part2

