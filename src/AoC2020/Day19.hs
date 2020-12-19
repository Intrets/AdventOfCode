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
  Left s -> void $ string s
  Right r ->
    foldl1 (<|>)
      . map (foldl1 ((=<<) . const) . reverse . map (makeRule rules))
      $ r

test :: [ReadP ()] -> ReadP ()
test = sequence_


main :: IO ()
main = do
  (rules1, tail -> messages) <- span (/= "") . lines <$> readFile
    "src/AoC2020/Day19.txt"

  let rules = M.fromList $ map (fst . head . readP_to_S (parser <* eof)) rules1

  let rule  = makeRule rules 0

  let checked =
        map (\message -> (message, readP_to_S (rule <* eof) message)) messages

  let ans = filter (not . null . readP_to_S (rule <* eof)) messages

  print $ length ans

  let rules2 = foldl
        (flip . uncurry $ M.insert)
        rules
        [(8, Right [[42], [42, 8]]), (11, Right [[42, 31], [42, 11, 31]])]

  let rule2 = makeRule rules2 0

  let ans2 = filter (not . null . readP_to_S (rule2 <* eof)) messages
  print $ length ans2

  putStrLn "day19"
