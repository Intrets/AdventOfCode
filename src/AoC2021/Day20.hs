{-# LANGUAGE ViewPatterns #-}
module AoC2021.Day20 where

import           Data.List
import qualified Data.Vector                   as V
import           Data.Bool
import           Data.List.Split

inputFile = readFile "src/AoC2021/Day20.txt"

getIndex :: String -> Int
getIndex = foldl1 ((+) . (2 *)) . map (bool 0 1 . (== '#'))


main :: IO ()
main = do
  [concat -> enhanceList, seed] <- splitOn [""] . lines <$> inputFile

  let enhance = (V.!) $ V.fromList enhanceList

  let pad n = foldl1 (.) $ replicate 2 $ transpose . map
        ((replicate n '.' ++) . (++ replicate n '.'))

  let part1Board = pad 4 seed

  let group3 = map (zipWith3 (\a b c -> [a, b, c]) <*> tail <*> tail . tail)

  let groupNeighbours =
        map (map concat) . transpose . group3 . transpose . group3

  let step = map (map (enhance . getIndex)) . groupNeighbours

  let solve n = length . filter (== '#') . concat . (!! n) . iterate step . pad (n * 2) $ seed

  putStr "part 1: " >> print (solve 2)
  putStr "part 2: " >> print (solve 50)

