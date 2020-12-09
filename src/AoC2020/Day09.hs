module AoC2020.Day09 where

import           Data.List
import           Data.Maybe
import           Control.Monad
import           Control.Monad.State

data Queue a = Queue [a] [a]

unwrap (Queue a b) = a ++ reverse b

pop :: State (Queue a) a
pop = state pop'
 where
  pop' (Queue []         []) = undefined
  pop' (Queue []         b ) = pop' $ Queue (reverse b) []
  pop' (Queue (a : rest) b ) = (a, Queue rest b)

push :: a -> State (Queue a) ()
push = state . (\c (Queue a b) -> ((), Queue a (c : b)))

solve :: Int -> Int -> [Int] -> State (Queue Int) [Int]
solve target total (n : numbers)
  | total == target = gets unwrap
  | total < target = push n >> solve target (n + total) numbers
  | total > target = do
    m <- pop
    solve target (total - m) (n : numbers)

main :: IO ()
main = do
  inputData <- map read . lines <$> readFile
    "src/AoC2020/Day09.txt"

  let number =
        head
          . fromJust
          . find (\(a : b) -> a `notElem` (sum <$> replicateM 2 b))
          . map (take 26)
          . drop 26
          . reverse
          . tails
          . reverse
          $ inputData

  putStr "part one: "
  print number

  putStr "part two: "
  print $ (+) <$> minimum <*> maximum $ evalState (solve number 0 inputData)
                                                  (Queue [] [])

