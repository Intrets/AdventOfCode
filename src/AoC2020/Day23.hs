module AoC2020.Day23 where

import           Data.Char
import           Data.List
import           System.IO.Unsafe
import           Control.Arrow
import qualified Data.Array.ST                 as STA
import           Control.Monad.ST
import           Control.Monad
import           Data.STRef.Strict
import qualified Data.Array.Unboxed            as A
import           Data.Tuple

data Queue a = Queue [a] [a] deriving (Show)

pop (Queue []       a ) = pop (Queue (reverse a) [])
pop (Queue (a : as) bs) = (a, Queue as bs)

popN n q = first reverse $ popN' n q
 where
  popN' 0 q = ([], q)
  popN' n q =
    let (r1, q1) = popN' (n - 1) q
        (r2, q2) = pop q1
    in  (r2 : r1, q2)

playGame1 :: [Int] -> [Int]
playGame1 (current : cups) = place1 destination picked rest ++ [current]
 where
  (picked, rest) = splitAt 3 cups
  destination =
    head . dropWhile (`elem` picked) . drop 1 . dropWhile (> current) $ cycle
      [9, 8 .. 1]

place1 :: Int -> [Int] -> [Int] -> [Int]
place1 destination picked (a : rest)
  | a == destination = a : picked ++ rest
  | otherwise        = a : place1 destination picked rest

solve2 :: Int -> Int -> Int -> [Int] -> Integer
solve2 start size steps begin =
  product . map fromIntegral . take 2 . drop 1 $ iterate (a A.!) 1
 where
  pairs = ap zip tail $ begin ++ [length begin + 1 .. size] ++ [head begin]
  a     = STA.runSTUArray $ do
    forward <- STA.newArray (1, size) 0 :: ST s (STA.STUArray s Int Int)
    forM_ pairs (uncurry (STA.writeArray forward))

    currentRef <- newSTRef (start :: Int)

    replicateM_ steps $ do
      current <- readSTRef currentRef
      next1   <- STA.readArray forward current
      next2   <- STA.readArray forward next1
      next3   <- STA.readArray forward next2
      next4   <- STA.readArray forward next3

      STA.writeArray forward current next4
      writeSTRef currentRef next4

      let Just destination =
            find (not . (`elem` [current, next1, next2, next3]))
              . iterate (\x -> 1 + ((x + size - 2) `mod` size))
              $ current

      destinationNext <- STA.readArray forward destination
      STA.writeArray forward destination next1
      STA.writeArray forward next3 destinationNext
    pure forward

main :: IO ()
main = do
  inputData <-
    map ((read :: String -> Int) . pure) . filter isNumber <$> readFile
      "src/AoC2020/Day23.txt"

  let part1 =
        take 8
          . drop 1
          . dropWhile (/= 1)
          . cycle
          . (!! 100)
          . iterate playGame1
          $ inputData
  putStr "part one: "  
  mapM_ (putStr . show) part1 >> putStrLn ""

  putStr "part two: "
  print $ solve2 (head inputData) 1000000 10000000 inputData

