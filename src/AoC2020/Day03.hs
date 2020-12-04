module AoC2020.Day03 where

import           Data.List

main :: IO ()
main = do
  inputData <- map cycle . lines <$> readFile "src/AoC2020/Day03.txt"

  putStr "part one: " >> print (solve inputData 3 1)

  putStr "part two: " >> print
    (product $ map (uncurry $ solve inputData)
                   [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    )

f :: Int -> Int -> [String -> String]
f x y =
  intercalate (replicate (pred y) $ const [])
    $ map (pure . (pure .) . (head .) . foldl (.) id)
    . inits
    . repeat
    $ drop x

solve :: [String] -> Int -> Int -> Int
solve s x y = length . filter (== '#') . concat . zipWith ($) (f x y) $ s
