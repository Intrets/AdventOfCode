{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module AoC2020.Day13 where

import           Data.List
import           Data.List.Split
import           Text.Read                      ( readMaybe )
import           Data.Maybe
import           Control.Monad
import           Data.Ord
import           Control.Arrow

solve1 :: Integer -> Integer -> Integer
solve1 n m = (m - (n `mod` m)) `mod` m

linearDiophantine :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)
linearDiophantine a b c | c `mod` g == 0 = Just (x0 * signum a, y0 * signum b)
                        | otherwise      = Nothing
 where
  (g, xg, yg) = euclid (abs a) (abs b)
  x0          = xg * (c `div` g)
  y0          = yg * (c `div` g)


solve2 :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
solve2 (b1, x1) (b2, x2) = (b1 * b2 `div` gcd b1 (-b2), x2 - b2 * k20)
  where Just (k10, k20) = linearDiophantine b1 (-b2) (x1 - x2)

euclid :: Integer -> Integer -> (Integer, Integer, Integer)
euclid a 0 = (a, 1, 0)
euclid a b =
  let (d, x1, y1) = euclid b (a `mod` b) in (d, y1, (x1 - y1 * (a `div` b)))

main :: IO ()
main = do
  [(read -> time), (map readMaybe . splitOn "," -> inputData)] <- lines
    <$> readFile "src/AoC2020/Day13.txt"

  putStr "part one: "
  print
    $ uncurry (*)
    . minimumBy (comparing snd)
    . mapMaybe (fmap (ap (,) (solve1 time)))
    $ inputData

  putStr "part two: "
  let pairs =
        uncurry zip
          . second (scanl (+) 0)
          . unzip
          . map ((,) <$> fromJust . head <*> (fromIntegral . length))
          . groupBy (const isNothing)
          $ inputData

  print $ (\(a, b) -> (1 + (b `div` a)) * a - b) . foldl1 solve2 $ pairs
