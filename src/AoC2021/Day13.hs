
module AoC2021.Day13 where

import           Control.Monad
import           Data.List.Split
import           Text.ParserCombinators.ReadP
import           Data.Char
import           Data.Functor
import           Data.List
import           Data.Bifunctor
import           Control.Applicative            ( (<|>) )
import qualified Data.Set                      as S

inputFile = readFile "src/AoC2021/Day13.txt"

parseFold :: ReadP [(Int, Int) -> (Int, Int)]
parseFold = many
  (do
    axis <-
      (string "fold along x=" $> first) <|> (string "fold along y=" $> second)
    number <- read <$> munch isDigit
    return $ axis ((number -) . abs . (number -))
  )

main :: IO ()
main = do
  [points_, folds_] <- splitOn [""] . lines <$> inputFile

  let points = map ((\[x, y] -> (read x, read y)) . splitOn ",") points_
  let folds = fst . head . readP_to_S (parseFold <* eof) $ concat folds_

  let part1 = length . group . sort . map (head folds) $ points

  let part2 = map head . group . sort . foldl (flip map) points $ folds
  let pointSet = S.fromList part2

  putStr "part 1: " >> print part1
  putStr "part 2:\n"

  forM_ [0 .. 5] $ \y -> do
    forM_ [0 .. 38]
      $ \x -> if (x, y) `S.member` pointSet then putStr "@" else putStr " "
    putStrLn ""
