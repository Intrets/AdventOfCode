{-# LANGUAGE ViewPatterns #-}
module AoC2020.Day19 where

import           Text.ParserCombinators.ReadP
import           Data.Char
import           Control.Applicative
import           Control.Monad
import           Data.List
import qualified Data.Map                      as M

parseInt :: ReadP Int
parseInt = read <$> munch1 isDigit

parser :: ReadP (Int, Either String [[Int]])
parser = do
  n <- parseInt
  string ": "
  rule <- (Left <$> stringRule) <|> (Right <$> combinedRule)
  pure (n, rule)

stringRule :: ReadP String
stringRule = char '"' *> munch1 isAlpha <* char '"'

combinedRule :: ReadP [[Int]]
combinedRule = sepBy1 (sepBy1 parseInt (char ' ')) (string " | ")

makeRule :: M.Map Int (Either String [[Int]]) -> Int -> ReadP ()
makeRule rules n = case rules M.! n of
  Left  s -> void $ string s
  Right r -> foldl1 (<|>) . map (mapM_ (makeRule rules)) $ r

main :: IO ()
main = do
  (rules1, tail -> messages) <- span (/= "") . lines <$> readFile
    "src/AoC2020/Day19.txt"

  let rules = M.fromList $ map (fst . head . readP_to_S (parser <* eof)) rules1

  putStr "part one: "
  print
    $ length
    . filter (not . null . readP_to_S (makeRule rules 0 <* eof))
    $ messages

  putStr "part two: "
  let updatedRules = foldl
        (flip . uncurry $ M.insert)
        rules
        [(8, Right [[42], [42, 8]]), (11, Right [[42, 31], [42, 11, 31]])]

  print
    $ length
    . filter (not . null . readP_to_S (makeRule updatedRules 0 <* eof))
    $ messages
