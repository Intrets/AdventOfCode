
module AoC2021.Day14 where

import           Text.ParserCombinators.ReadP
import           Data.List
import           Data.Char
import           Control.Monad
import qualified Data.Map.Strict               as M
import           Control.Monad.State
import           Data.Function
import           Control.Arrow

inputFile = readFile "src/AoC2021/Day14.txt"

parse :: ReadP (String, [((Char, Char), Char)])
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

solve mapping steps start =
  flip evalState (foldr (flip (M.insertWith (+)) 1) M.empty start) $ do
    replicateM_ steps $ do
      pairs <- gets M.toList
      put M.empty
      forM_ pairs $ \((a, b), n) -> do
        modify (M.insertWith (+) (a, mapping (a, b)) n)
        modify (M.insertWith (+) (mapping (a, b), b) n)
    gets $ map (first fst) . M.toList


main :: IO ()
main = do
  input <- inputFile
  let (start, rules) = fst . head . readP_to_S (parse <* eof) $ input
  let mapping        = (M.!) . M.fromList $ rules

  let pairs          = ap zip tail start

  let solve2 n =
        ((-) <$> maximum <*> minimum)
          .  map (sum . map snd)
          .  groupBy ((==) `on` fst)
          .  sortOn fst
          $  [(head start, 1), (last start, 1)]
          ++ solve mapping n pairs

  putStr "part 1: " >> print (solve2 10)
  putStr "part 2: " >> print (solve2 40)

