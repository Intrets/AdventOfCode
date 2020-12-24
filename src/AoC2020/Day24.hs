{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module AoC2020.Day24 where

import           Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP  as P
import           Control.Applicative
import           Data.List
import qualified Data.Array                    as A
import qualified Data.Graph                    as G
import           Control.Monad
import qualified Data.Vector.Unboxed           as VU

neighbours = [(1, 0), (1, -1), (0, -1), (-1, 0), (-1, 1), (0, 1)]
add (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

parser :: ReadP [(Int, Int)]
parser = P.many $ foldl1 (<|>) $ zipWith
  (<$)
  neighbours
  (map string ["e", "se", "sw", "w", "nw", "ne"])

main :: IO ()
main = do
  inputData <- lines <$> readFile "src/AoC2020/Day24.txt"

  let flips =
        map (foldl1 add . fst . head . readP_to_S (parser <* eof)) inputData

  putStr "part one: "
  print $ length . filter odd . map length . group . sort $ flips

  putStr "part two: "

  let size = 100
  let toIndex (x, y) = (x + size) * 2 * size + y + size
  let adjacencyList =
        concat
          $   (\x y ->
                map (((toIndex (x, y), ) . toIndex) . add (x, y)) neighbours
              )
          <$> [-size + 2 .. size - 2]
          <*> [-size + 2 .. size - 2]
  let graph = G.buildG (0, 2 * size * 2 * size - 1) adjacencyList

  let state = VU.accum (const . not)
                       (VU.replicate (2 * size * 2 * size) False)
                       (map ((, undefined) . toIndex) flips)

  print $ VU.length . VU.filter id . (!! 100) . iterate (step graph) $ state

step :: G.Graph -> VU.Vector Bool -> VU.Vector Bool
step graph state = VU.imap
  (\i -> \case
    True | getNeighbourCount i `elem` [1, 2] -> True
         | otherwise                         -> False
    False | getNeighbourCount i == 2 -> True
          | otherwise                -> False
  )
  state
 where
  getNeighbourCount =
    length . filter id . map (state `VU.unsafeIndex`) . (graph A.!)

