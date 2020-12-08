module AoC2020.Day08 where

import           Data.List
import qualified Data.Vector                   as V
import           Control.Monad.State
import qualified Data.IntSet                   as IS
import qualified Data.Graph                    as G
import           Data.Maybe

run :: V.Vector (String, Int) -> Int -> State IS.IntSet Int
run ops current = do
  visited <- gets (IS.member current)
  if visited
    then pure 0
    else modify (IS.insert current) >> case ops V.!? current of
      Just ("nop", _) -> run ops (current + 1)
      Just ("acc", i) -> (i +) <$> run ops (current + 1)
      Just ("jmp", i) -> run ops (current + i)
      Nothing         -> pure 0

main :: IO ()
main = do
  ops <-
    V.fromList
    .   map ((\[a, b] -> (a, read (filter (/= '+') b))) . words)
    .   lines
    <$> readFile "src/AoC2020/Day08.txt"

  putStr "part one: "
  print $ flip evalState IS.empty $ run ops 0

  putStr "part two: "

  let edges =
        zipWith
            (\i (op, d) -> case op of
              "nop" -> (i, min (i + 1) (length ops))
              "acc" -> (i, min (i + 1) (length ops))
              "jmp" -> (i, min (i + d) (length ops))
            )
            [0 ..]
          . V.toList
          $ ops

  let graph = G.buildG (0, length ops) edges
  let isTarget = (||) <$> (>= length ops) <*> flip
        IS.member
        (IS.fromList . G.reachable (G.transposeG graph) $ length ops)
  let
    change =
      mapMaybe
          (\node -> case ops V.! node of
            ("nop", i) ->
              if isTarget (node + i) then Just (node, ("jmp", i)) else Nothing
            ("acc", i) -> Nothing
            ("jmp", i) ->
              if isTarget (node + 1) then Just (node, ("nop", i)) else Nothing
          )
        $ G.reachable graph 0

  print $ flip evalState IS.empty $ run (ops V.// change) 0
