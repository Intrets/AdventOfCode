{-# LANGUAGE ViewPatterns #-}
module AoC2020.Day21 where

import           Text.ParserCombinators.ReadP
import           Data.Char
import qualified Data.Set                      as S
import           Data.Tuple
import           Data.List
import           Data.Function
import qualified Data.Map                      as M
import           Data.Ord
import           Control.Arrow

parser :: ReadP [([String], [String])]
parser =
  sepBy
      (do
        ingredients <- sepBy (munch isAlpha) (char ' ')
        string " (contains "
        allergens <- sepBy (munch isAlpha) (string ", ")
        char ')'
        pure (ingredients, allergens)
      )
      (char '\n')
    <* char '\n'

getMatches :: [(String, S.Set String)] -> [(String, String)]
getMatches [] = []
getMatches (sortOn (S.size . snd) -> ((allergen, head . S.toList -> ingredient):rest))
  = (ingredient, allergen)
    : (getMatches . map (second $ S.delete ingredient) $ rest)

main :: IO ()
main = do
  inputData <- fst . head . readP_to_S (parser <* eof) <$> readFile
    "src/AoC2020/Day21.txt"

  let ingredients = S.fromList . concatMap fst $ inputData
  let ingredientsCounts =
        foldl (\m e -> M.insertWith (+) e 1 m) M.empty
          . concatMap fst
          $ inputData

  let p =
        groupBy ((==) `on` snd)
          . sortOn snd
          . concatMap (uncurry $ map . (,))
          $ inputData

  let p1 =
        S.difference ingredients
          . S.unions
          . map (foldl1 S.intersection . map (S.fromList . fst))
          $ p

  putStr "part one: "
  print $ sum . map (ingredientsCounts M.!) . S.toList $ p1

  putStr "part two: "
  putStrLn
    $ intercalate ","
    . map fst
    . sortOn snd
    . getMatches
    . map
        ((,) <$> snd . head <*> foldl1 S.intersection . map (S.fromList . fst))
    $ p








