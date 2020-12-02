module AoC2020.Day02 where

import           Data.Function
import           Data.Char
import           Text.ParserCombinators.ReadP

parser :: (Int -> Int -> Char -> String -> Bool) -> ReadP Int
parser f = length . filter id <$> many
  (   f
  <$> (read <$> munch isDigit)
  <*  char '-'
  <*> (read <$> munch isDigit)
  <*  skipSpaces
  <*> get
  <*  char ':'
  <*  skipSpaces
  <*> munch isLetter
  <*  skipSpaces
  )

main :: IO ()
main = do
  inputData <- readFile "src/AoC2020/Day02.txt"

  let part1 a b c = ((&&) <$> (>= a) <*> (<= b)) . length . filter (== c)
  let part2 a b c = ((/=) `on` (== c)) <$> (!! (a - 1)) <*> (!! (b - 1))

  putStr "part one: "
    >> (print . fst . head $ readP_to_S (parser part1 <* eof) inputData)
  putStr "part two: "
    >> (print . fst . head $ readP_to_S (parser part2 <* eof) inputData)


