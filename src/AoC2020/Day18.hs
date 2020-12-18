module AoC2020.Day18 where

import           Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP  as P
import           Control.Applicative
import           Data.Char
import           Control.Monad

data Operator = Id | Mult | Add

parseInt :: ReadP Integer
parseInt = read <$> munch1 isDigit

doAddition
  :: [(Operator, Integer)] -> [(Operator, Integer)] -> [(Operator, Integer)]
doAddition e   []                   = e
doAddition res ((Mult, val) : rest) = doAddition ((Mult, val) : res) rest
doAddition ((op, prev) : res) ((Add, val) : rest) =
  doAddition ((op, val + prev) : res) rest

expression1 :: ReadP Integer
expression1 =
  (do
      char '('
      res <- chainl1 expression1 (((+) <$ char '+') <|> ((*) <$ char '*'))
      char ')'
      pure res
    )
    <|> parseInt

expression2 :: ReadP Integer
expression2 =
  (do
      char '('
      exp1 <- expression2
      rest <- P.many
        ((,) <$> ((Add <$ char '+') <|> (Mult <$ char '*')) <*> expression2)
      char ')'
      pure $ product . map snd . doAddition [(Id, exp1)] $ rest
    )
    <|> parseInt

main :: IO ()
main = do
  inputData <- map ((++ ")") . ('(' :)) . lines . filter (/= ' ') <$> readFile
    "src/AoC2020/Day18.txt"

  putStr "part one : "
  print
    $ sum
    . map fst
    . concatMap (readP_to_S (expression1 <* eof))
    $ inputData

  putStr "part two : "
  print
    $ sum
    . map fst
    . concatMap (readP_to_S (expression2 <* eof))
    $ inputData

