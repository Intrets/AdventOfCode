{-# LANGUAGE LambdaCase #-}

module AoC2020.Day04 where

import           Text.ParserCombinators.ReadP
import           Data.Char
import           Data.List
import           Control.Monad
import           Data.Function
import           Control.Applicative

parser1 :: ReadP [(String, String)]
parser1 = sepBy ((,) <$> munch isAlpha <* char ':' <*> munch (not . isSpace))
                (string " " +++ string "\n")

parser2 :: String -> ReadP Bool
parser2 = \case
  "byr" -> liftA2 (&&) (>= 1920) (<= 2002) . read <$> munch isDigit
  "iyr" -> liftA2 (&&) (>= 2010) (<= 2020) . read <$> munch isDigit
  "eyr" -> liftA2 (&&) (>= 2020) (<= 2030) . read <$> munch isDigit
  "hgt" -> do
    num <- read <$> munch isDigit
    (string "cm" >> pure (num >= 150 && num <= 193))
      <++ (string "in" >> pure (num >= 59 && num <= 76))
      <++ pure False
  "hcl" ->
    option False
      $  char '#'
      >> count 6 (satisfy (liftA2 (||) isNumber (`elem` ['a' .. 'f'])))
      >> pure True
  "ecl" ->
    choice (map string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
      >> pure True
  "pid" -> count 9 (satisfy isDigit) >> pure True
  "cid" -> munch (const True) >> pure True
  _     -> undefined

solve1 :: [(String, String)] -> Bool
solve1 = ap ((||) `on` (== 8) . length . group . sort) (++ ["cid"]) . map fst

solve2 :: [(String, String)] -> Bool
solve2 =
  all (uncurry $ (any (liftA2 (&&) fst (null . snd)) .) . readP_to_S . parser2)

main :: IO ()
main = do
  inputData <-
    fst . last . readP_to_S (sepBy parser1 (string "\n\n")) <$> readFile
      "src/AoC2020/Day04.txt"

  putStr "part one: " >> print (length . filter solve1 $ inputData)
  putStr "part two: "
    >> print (length . filter solve2 . filter solve1 $ inputData)







