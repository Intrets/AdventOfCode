module AoC2020.Day15 where

import           Data.List.Split
import           Control.Monad.ST.Strict
import qualified Data.Array.ST                 as STA
import           Control.Monad
import           Data.List

solve :: Int -> [Int] -> Int
solve target start = runST $ do
  array <- STA.newArray (0, target) (-1) :: ST s (STA.STUArray s Int Int)

  forM_ (init . flip zip [1 ..] $ start) $ uncurry (STA.writeArray array)

  foldM
    (\n i -> do
      j <- STA.readArray array n
      STA.writeArray array n i
      pure $ case j of
        -1 -> 0
        j  -> i - j
    )
    (last start)
    [(length start) .. target - 1]

main :: IO ()
main = do
  let inputData = map read . splitOn "," $ "5,1,9,18,13,8,0"

  putStr "part two: "
  print $ solve 2020 inputData

  putStr "part one: "
  print $ solve 30000000 inputData
