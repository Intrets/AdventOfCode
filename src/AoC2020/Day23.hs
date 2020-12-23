module AoC2020.Day23 where

import           Data.Char
import           Data.List
import           System.IO.Unsafe
import           Control.Arrow
import qualified Data.Array.ST                 as AS
import           Control.Monad.ST

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

  mapM_ (putStr . show) part1 >> putStrLn ""

  putStrLn "part two: "


  putStrLn "day23"
