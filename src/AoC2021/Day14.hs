{-# LANGUAGE TupleSections #-}
module AoC2021.Day14 where

import           Text.ParserCombinators.ReadP
import           Data.List
import           Data.Char
import           Control.Monad
import qualified Data.Map                      as M
import           Control.Monad.State
import           Data.Function
import           Control.Arrow

inputFile = readFile "src/AoC2021/Day14.txt"

parse = do
  start <- munch isAlpha
  skipSpaces

  rules <- many $ do
    [a, b] <- count 2 $ satisfy isAlpha
    string " -> "
    to <- satisfy isAlpha
    skipSpaces
    return ((a, b), to)
  return (start, rules)

step
  :: ((Char, Char) -> Char)
  -> [((Char, Char), Integer)]
  -> [((Char, Char), Integer)]
step mapping =
  map (foldl1 (\((a, b), n) (_, m) -> ((a, b), n + m)))
    . groupBy ((==) `on` fst)
    . sortOn fst
    . concatMap
        (\((a, b), n) -> [((a, mapping (a, b)), n), ((mapping (a, b), b), n)])



main :: IO ()
main = do
  input <- inputFile
  let (start, rules) = fst . head . readP_to_S (parse <* eof) $ input
  let mapping        = (M.!) . M.fromList $ rules

  let pairs          = map (, 1) . ap zip tail $ start
  let counts n =
        ((-) <$> maximum <*> minimum)
          . map (snd . foldl1 (\(a, n) (_, m) -> (a, n + m)))
          . groupBy ((==) `on` fst)
          . sortOn fst
          . (++ [(head start, 1), (last start, 1)])
          . map (first fst)
          . (!! n)
          . iterate (step mapping)
          $ pairs

  putStr "part 1: " >> print (counts 10)
  putStr "part 2: " >> print (counts 40)

