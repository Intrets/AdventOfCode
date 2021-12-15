module AoC2021.Day15 where

import           Data.List
import qualified Data.HashMap.Strict           as M
import           Control.Monad.State
import qualified Data.Set                      as S
import           Control.Arrow
import           Data.Bifunctor                 ( bimap )
import           Data.Ord
import qualified Data.Heap                     as H
import           Data.List.Split
import           Data.STRef.Strict
import           Control.Monad.ST.Strict
import qualified Data.Array.ST                 as STA
import qualified Data.Array.Unboxed            as A
import qualified Data.Array                    as AR

inputFile = readFile "src/AoC2021/Day15.txt"

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ f a = do
  b <- f
  when b $ a >> whileM_ f a

type Vec2 = (Int, Int)

solveST :: (Int, Int) -> (Vec2 -> Int) -> A.UArray (Int, Int) Int
solveST (width, height) getWeight = STA.runSTUArray $ do
  visited <-
    STA.newArray ((-1, -1), (width, height)) False :: ST
      s
      (STA.STUArray s (Int, Int) Bool)

  let edges =
        zip (repeat (-1)) [-1 .. height]
          ++ zip (repeat width) [-1 .. height]
          ++ zip [-1 .. width]  (repeat (-1))
          ++ zip [-1 .. width]  (repeat height)

  forM_ edges $ flip (STA.writeArray visited) True

  distances <-
    STA.newArray ((0, 0), (width - 1, height - 1)) maxBound :: ST
      s
      (STA.STUArray s (Int, Int) Int)
  STA.writeArray distances (0, 0) 0

  front <- newSTRef (S.singleton (0, (0, 0)) :: S.Set (Int, (Int, Int)))

  whileM_ (not . S.null <$> readSTRef front) $ do
    ((value, point), s) <- S.deleteFindMin <$> readSTRef front
    writeSTRef front s

    pointVisited <- STA.readArray visited point

    unless pointVisited $ do
      STA.writeArray visited point True
      let neighbours = map ($point) ([first, second] <*> [succ, pred])

      forM_ neighbours $ \p -> do
        pVisited <- STA.readArray visited p
        unless pVisited $ do
          v <- STA.readArray distances p
          let newValue = min v (value + getWeight p)
          modifySTRef' front $ S.insert (newValue, p)
          STA.writeArray distances p newValue

  return distances


main :: IO ()
main = do
  weights <- map (map (read . pure)) . lines <$> inputFile
  let width  = length . head $ weights
  let height = length weights

  let points = (,) <$> [0 .. width - 1] <*> [0 .. height - 1]
  let pairs  = zip points (concat weights)

  let getWeight1 =
        (AR.!) $ AR.accumArray (+) 0 ((0, 0), (width - 1, height - 1)) pairs

  let part2Weights =
        concatMap
            (\(x, y) -> map
              (bimap (bimap (+ x * width) (+ y * height))
                     (succ . (`mod` 9) . pred . (+ (x + y)))
              )
              pairs
            )
          $   (,)
          <$> [0 .. 4]
          <*> [0 .. 4]

  let getWeight2 = (AR.!) $ AR.accumArray
        (+)
        0
        ((0, 0), (width * 5 - 1, height * 5 - 1))
        part2Weights


  let part1 = solveST (width, height) getWeight1
  let part2 = solveST (width * 5, height * 5) getWeight2

  putStr "part 1: " >> print (part1 A.! (width - 1, height - 1))
  putStr "part 2: " >> print (part2 A.! (width * 5 - 1, height * 5 - 1))


