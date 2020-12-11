module AoC2020.Day11 where

import           Data.List
import           Control.Monad

step = map (map cell)

cell :: [String] -> Char
cell [a, [b, 'L', c], d] | '#' `notElem` (b : c : (a ++ d)) = '#'
                         | otherwise                        = 'L'
cell [a, [b, '#', c], d]
  | (>= 4) . length . filter (== '#') $ (b : c : (a ++ d)) = 'L'
  | otherwise = '#'
cell _ = '.'

f = zipWith3 (zipWith3 (\a b c -> [a, b, c])) `ap` tail `ap` (tail . tail)

testList 0 _ = []
testList n m = testList (n - 1) m ++ [take m [n * m ..]]

main :: IO ()
main = do
  inputData <-
    (=<<) (flip (++)) (pure . flip replicate '.' . length . head)
    .   (=<<) (:) (flip replicate '.' . length . head)
    .   map ((++ ".") . ('.' :))
    .   lines
    <$> readFile "src/AoC2020/Day11.txt"

  inputData2 <- lines <$> readFile "src/AoC2020/Day11.txt"

  let pad =
        (=<<) (flip (++)) (pure . flip replicate '.' . length . head)
          . (=<<) (:) (flip replicate '.' . length . head)
          . map ((++ ".") . ('.' :))

  let grouper = map (map transpose) . transpose . f . transpose . f . pad

  let grouper2 = map f . f . pad

  let step2 = step . grouper


  print ""

  ----------------------------------------
  
  let run = iterate step2 inputData2

  let diff = dropWhile (uncurry (/=)) . ap zip tail $ run

  print "answer"

  print $ length . filter (=='#') . concat .  fst . head $ diff


test :: [a] -> [(a, a)]
test = ap zip tail

test2 :: [a] -> [(a, a, a)]
test2 = zip3 `ap` tail `ap` (tail . tail)

