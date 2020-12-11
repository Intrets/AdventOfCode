{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module AoC2020.Day11 where

import           Data.List
import           Control.Monad
import           Data.Tuple
import           Control.Arrow
import qualified Data.Graph                    as G
import qualified Data.Vector.Unboxed           as V
import qualified Data.Array                    as A
import           Data.List.Split
import           Data.Function

testList 0 _ = []
testList n m = testList (n - 1) m ++ [take m [(n - 1) * m ..]]

main :: IO ()
main = do
  inputData <- lines <$> readFile "src/AoC2020/Day11.txt"

  let numberedInput = zipWith
        zip
        (testList (length inputData) (length . head $ inputData))
        inputData

  let state   = V.fromList . concat $ inputData

  let rows    = numberedInput
  let columns = transpose numberedInput
  let diagonals1 =
        transpose . zipWith ($) (iterate (((-1, '.') :) .) id) $ rows
  let diagonals2 =
        transpose . zipWith ($) (iterate (((-1, '.') :) .) id) $ reverse rows

  let pairs1 =
        map (fst *** fst)
          .  filter (uncurry ((&&) `on` ((/= '.') . snd)))
          .  concatMap (ap zip tail)
          $  rows
          ++ columns
          ++ diagonals2
          ++ diagonals1

  let pairs =
        map (fst *** fst)
          .  concatMap (ap zip tail . filter ((`elem` "#L") . snd))
          $  rows
          ++ columns
          ++ diagonals2
          ++ diagonals1

  let graph = G.buildG (0, maximum . map (uncurry max) $ pairs)
        $ ap (++) (map swap) pairs

  let graph1 = G.buildG (0, maximum . map (uncurry max) $ pairs)
        $ ap (++) (map swap) pairs1

  let run   = map V.toList . iterate (step3 5 graph) $ state
  let diff  = fst . head . dropWhile (uncurry (/=)) . ap zip tail $ run

  let run1  = map V.toList . iterate (step3 4 graph1) $ state
  let diff1 = fst . head . dropWhile (uncurry (/=)) . ap zip tail $ run1


  print $ length . filter (== '#') $ diff1
  print $ length . filter (== '#') $ diff

  print ""

step3 :: Int -> G.Graph -> V.Vector Char -> V.Vector Char
step3 x graph state =
  V.map
      (\case
        (_, '.') -> '.'
        (getNeighbours -> e, '#') | (>= x) . length . filter (== '#') $ e -> 'L'
                                  | otherwise                             -> '#'
        (getNeighbours -> e, 'L') | notElem '#' e -> '#'
                                  | otherwise     -> 'L'
      )
    . V.indexed
    $ state
  where getNeighbours = map (state V.!) . (graph A.!)


