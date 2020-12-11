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
import           Data.Function

numbered 0 _ = []
numbered n m = numbered (n - 1) m ++ [take m [(n - 1) * m ..]]

main :: IO ()
main = do
  inputData <- lines <$> readFile "src/AoC2020/Day11.txt"

  let numberedInput = zipWith
        zip
        (numbered (length inputData) (length . head $ inputData))
        inputData

  let state   = V.fromList . concat $ inputData

  let rows    = numberedInput
  let columns = transpose numberedInput
  let diagonals1 =
        transpose . zipWith ($) (iterate (((-1, '.') :) .) id) $ rows
  let diagonals2 =
        transpose . zipWith ($) (iterate (((-1, '.') :) .) id) $ reverse rows

  let rawPairs = rows ++ columns ++ diagonals1 ++ diagonals2

  let pairs1 =
        map (fst *** fst)
          . filter (uncurry ((&&) `on` ((/= '.') . snd)))
          . concatMap (ap zip tail)
          $ rawPairs

  let pairs2 =
        map (fst *** fst)
          . concatMap (ap zip tail . filter ((`elem` "#L") . snd))
          $ rawPairs

  putStr "part one: "
  print $ solve 4 pairs1 state

  putStr "part two: "
  print $ solve 5 pairs2 state

solve :: Int -> [(Int, Int)] -> V.Vector Char -> Int
solve x pairs state =
  let graph = G.buildG (0, maximum . map (uncurry max) $ pairs)
        $ ap (++) (map swap) pairs
  in  length
        . filter (== '#')
        . fst
        . head
        . dropWhile (uncurry (/=))
        . ap zip tail
        . map V.toList
        . iterate (step x graph)
        $ state

step :: Int -> G.Graph -> V.Vector Char -> V.Vector Char
step x graph state =
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

