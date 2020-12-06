module AoC2020.Day06 where

import           Text.ParserCombinators.ReadP  as P
import           Data.Char
import           Data.List
import           Control.Applicative
import           Control.Monad

parser :: ReadP [[String]]
parser = P.many $ lines <$> manyTill get (void (string "\n\n") <++ eof)

main :: IO ()
main = do
  inputData <- fst . head . readP_to_S (parser <* eof) <$> readFile
    "src/AoC2020/Day06.txt"

  let part1 = length . group . sort . concat

  putStr "part one: " >> print (sum . map part1 $ inputData)

  let part2 = length
        . liftA2 filter (((==) .) length) (map length . group . sort . concat)

  putStr "part two: " >> print (sum . map part2 $ inputData)

