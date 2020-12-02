module AoC2020.Day02 where

import           Text.ParserCombinators.Parsec
import           Data.Function

parser :: (Int -> Int -> Char -> String -> Bool) -> Parser Int
parser f = length . filter id <$> many
  (   f
  <$> (read <$> many digit)
  <*  char '-'
  <*> (read <$> many digit)
  <*  spaces
  <*> letter
  <*  char ':'
  <*  spaces
  <*> many letter
  <*  newline
  )

main :: IO ()
main = do
  inputData <- readFile "src/AoC2020/Day02.txt"

  let part1 a b c = ((&&) <$> (>= a) <*> (<= b)) . length . filter (== c)
  let part2 a b c = ((/=) `on` (== c)) <$> (!! (a - 1)) <*> (!! (b - 1))

  putStr "part one: " >> print (parse (parser part1) "" inputData)
  putStr "part two: " >> print (parse (parser part2) "" inputData)



