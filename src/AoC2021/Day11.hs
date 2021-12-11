module AoC2021.Day11 where

import           Data.List
import           Control.Monad
import           Data.List.Split
import           Data.Bifunctor
import           Control.Monad.ST
import qualified Data.Array.ST                 as STA

whileM :: Monad m => (a -> Bool) -> m a -> m [a]
whileM f action = do
  a <- action
  if f a then (a :) <$> whileM f action else return [a]

inputFile = readFile "src/AoC2021/Day11.txt"

solve :: [[Int]] -> [Int]
solve squid = runST $ do
  let width  = length . head $ squid
  let height = length squid
  array <-
    STA.newListArray ((0, 0), (width - 1, height - 1)) (concat squid) :: ST
      s
      (STA.STArray s (Int, Int) Int)

  let flood array i = do
        e <- STA.readArray array i
        STA.writeArray array i $ succ e
        if e == 9
          then succ . sum <$> mapM
            (flood array . ($ i))
            (tail $ bimap <$> [id, succ, pred] <*> [id, succ, pred])
          else return 0

  whileM (/= (width - 2) * (height - 2)) $ do
    total <- sum
      <$> forM ((,) <$> [0 .. width - 1] <*> [0 .. height - 1]) (flood array)

    forM_ ((,) <$> [1 .. width - 2] <*> [1 .. height - 2]) $ \i -> do
      e <- STA.readArray array i
      when (e > 9) $ STA.writeArray array i 0

    return total

main :: IO ()
main = do
  squid <-
    map ((++ [minBound]) . ([minBound] ++) . map (read . pure))
    .   lines
    <$> inputFile
  let width = length . head $ squid
  let paddedSquid =
        [replicate width minBound] ++ squid ++ [replicate width minBound]
  let height = length paddedSquid

  let part1  = sum . take 100 . solve $ paddedSquid
  let part2  = length . solve $ paddedSquid


  putStr "part 1: " >> print part1
  putStr "part 2: " >> print part2

