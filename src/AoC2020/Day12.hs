{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module AoC2020.Day12 where

data P a = P !a !a deriving (Show, Eq)

instance Num a => Num (P a) where
  (P x1 y1) * (P x2 y2) = P (x1 * x2) (y1 * y2)
  (P x1 y1) + (P x2 y2) = P (x1 + x2) (y1 + y2)
  (P x1 y1) - (P x2 y2) = P (x1 - x2) (y1 - y2)
  abs (P x y) = P (abs x) (abs y)
  signum      = undefined
  fromInteger = undefined

fstP (P x _) = x
sndP (P _ y) = y
swap (P x y) = P y x
manhattan (abs -> P x y) = x + y

turnL :: Int -> P Int -> P Int
turnL d p@(P x y) = case (d + 360) `mod` 360 of
  0   -> p
  90  -> P (-y) x
  180 -> P (-x) (-y)
  270 -> P y (-x)

main :: IO ()
main = do
  inputData <- lines <$> readFile "src/AoC2020/Day12.txt"

  let answer1 = foldl
        (\(P d p) -> \case
          'N' : (read -> v) -> P d (p + P 0 v)
          'E' : (read -> v) -> P d (p + P v 0)
          'S' : (read -> v) -> P d (p - P 0 v)
          'W' : (read -> v) -> P d (p - P v 0)
          'L' : (read -> v) -> P (turnL v d) p
          'R' : (read -> v) -> P (turnL (-v) d) p
          'F' : (read -> v) -> P d (p + (P v v * d))
        )
        (P (P 1 0) (P 0 0))
        inputData

  putStr "part one: "
  print $ manhattan . sndP $ answer1

  let answer2 = foldl
        (\(P d p) -> \case
          'N' : (read -> v) -> P (d + P 0 v) p
          'E' : (read -> v) -> P (d + P v 0) p
          'S' : (read -> v) -> P (d - P 0 v) p
          'W' : (read -> v) -> P (d - P v 0) p
          'L' : (read -> v) -> P (turnL v d) p
          'R' : (read -> v) -> P (turnL (-v) d) p
          'F' : (read -> v) -> P d (p + (P v v * d))
        )
        (P (P 10 1) (P 0 0))
        inputData

  putStr "part two: "
  print $ manhattan . sndP $ answer2
