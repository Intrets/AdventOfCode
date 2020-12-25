module AoC2020.Day25 where

import           Data.List

rng = ((`mod` 20201227) .) . (*)

findLoopSize x = succ . length . takeWhile (/= x) . iterate (rng 7)

loop n subject = (!! pred n) . iterate (rng subject)

main :: IO ()
main = do
  [card, door] <- map (read :: String -> Int) . lines <$> readFile
    "src/AoC2020/Day25.txt"

  print $ loop (findLoopSize card 7) door door

