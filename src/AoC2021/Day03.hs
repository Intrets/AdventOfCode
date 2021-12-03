{-# LANGUAGE TupleSections #-}
module AoC2021.Day03 where

import           Data.List
import           Data.Function
import           Data.Ord

inputFile = readFile "src/AoC2021/Day03.txt"

fromBinary = foldl (flip (((.) .) ((+) . (* 2))) (read . pure)) 0

main :: IO ()
main = do
  part1 <-
    product
    .   map fromBinary
    .   transpose
    .   map (map head . sortOn length . group . sort)
    .   transpose
    .   lines
    <$> inputFile

  putStr "part 1: " >> print part1

  let solvePart2 order (index, binary) =
        ( index + 1
        , order
          . sortOn length
          . groupBy ((==) `on` (!! index))
          . sortOn (!! index)
          $ binary
        )

  let p2 order =
        head
          . head
          . dropWhile (not . null . tail)
          . map snd
          . iterate (solvePart2 order)
          . (0, )
          . lines

  oxygen <- fromBinary . p2 last <$> inputFile
  co2    <- fromBinary . p2 head <$> inputFile

  putStr "part 2: " >> print (oxygen * co2)

