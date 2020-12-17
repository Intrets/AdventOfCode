{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module AoC2020.Day17 where

import           Control.Monad
import           Data.List
import qualified Data.Vector                   as V
import qualified Data.Vector                   as VU
import           Data.Tuple
import qualified Data.Graph                    as G
import qualified Data.Array.Unboxed            as A
import qualified Data.Map                      as M

main :: IO ()
main = do
  starts <-
    map fst
    .   filter snd
    .   map (\(x, y, val) -> ((x, y), val))
    .   concat
    .   zipWith (\x -> zipWith (x, , ) [-3 ..]) [-3 ..]
    .   map (map (== '#'))
    .   lines
    <$> readFile "src/AoC2020/Day17.txt"

  putStr "part one: "
  print $ (!! 6) . map (length . filter (== 1) . VU.toList) . solve 3 $ starts

  putStr "part two: "
  print $ (!! 6) . map (length . filter (== 1) . VU.toList) . solve 4 $ starts

solve m starts =
  let
    size = 10 :: Int
    pairs n = zip [0 :: Int ..] $ sequence
      (replicate 2 [-size .. size] ++ replicate (n - 2) [-size + 3 .. size - 3])
    fromIndex :: Int -> [Int] -> Int
    fromIndex = (M.!) . M.fromList . map swap . pairs

    adjacencyList n = map
      (   (,)
      <$> fromIndex n
      <*> (map (fromIndex n) . mapM (enumFromTo <$> pred <*> succ))
      )
      (sequence
        (  replicate 2       [-size + 1 .. size - 1]
        ++ replicate (n - 2) [-size + 4 .. size - 4]
        )
      )

    graphSize = length $ pairs m
    graph =
      G.buildG (0, graphSize)
        . concatMap (\(e, l) -> map (e, ) l)
        $ adjacencyList m
    state1 =
      VU.replicate graphSize 0
        VU.// map
                (\(x, y) -> (fromIndex m ([x, y] ++ replicate (m - 2) 0), 1))
                starts
  in
    iterate (step graph) state1


step :: G.Graph -> VU.Vector Int -> VU.Vector Int
step graph state = VU.imap
  (\i -> \case
    1 | getNeighbourCount i `elem` [3, 4] -> 1
      | otherwise                         -> 0
    0 | getNeighbourCount i == 3 -> 1
      | otherwise                -> 0
  )
  state
  where getNeighbourCount = sum . map (state `VU.unsafeIndex`) . (graph A.!)













