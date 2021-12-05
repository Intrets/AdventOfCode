module AoC2021.Day05 where

import           Text.ParserCombinators.ReadP
import           Data.Char
import           Data.List
import           Data.Bifunctor
import           Data.Bool
import           Control.Monad

inputFile = readFile "src/AoC2021/Day05.txt"

number :: ReadP Int
number = read <$> munch1 isDigit

parser :: ReadP [([Int], [Int])]
parser = many
  (do
    a <- number
    char ','
    b <- number
    string " -> "
    c <- number
    char ','
    d <- number
    skipSpaces
    pure ([a, b], [c, d])
  )

getRange :: (Enum a, Ord a) => (a, a) -> [a]
getRange =
  enumFromThenTo <$> fst <*> ap (bool pred succ . uncurry (<)) fst <*> snd

main :: IO ()
main = do
  lines <- fst . last . readP_to_S parser <$> inputFile

  let part1 =
        length
          . filter ((> 1) . length)
          . group
          . sort
          . concatMap (sequence . uncurry (zipWith $ curry getRange))
          . filter (or . uncurry (zipWith (==)))
          $ lines

  let part2 =
        length
          . filter ((> 1) . length)
          . group
          . sort
          . uncurry (++)
          . bimap (concatMap sequence) (concatMap transpose)
          . partition (any ((== 1) . length))
          . map (uncurry (zipWith $ curry getRange))
          $ lines

  putStr "part 1: " >> print part1
  putStr "part 2: " >> print part2

