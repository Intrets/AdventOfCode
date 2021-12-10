{-# LANGUAGE LambdaCase #-}
module AoC2021.Day10 where

import           Control.Monad.State
import           Data.Maybe
import           Data.List

inputFile = readFile "src/AoC2021/Day10.txt"

scorePart1 = \case
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137

scorePart2 = \case
  '(' -> 1
  '[' -> 2
  '{' -> 3
  '<' -> 4

push = modify . (:)
peek = gets head
pop = state $ (,) <$> head <*> tail

solve :: String -> (String, String)
solve parens = flip runState [] $ catMaybes <$> forM
  parens
  (\c -> if c `elem` "{[(<"
    then push c >> return Nothing
    else do
      top <- peek
      if [top, c] `notElem` ["()", "[]", "{}", "<>"]
        then return $ Just c
        else pop >> return Nothing
  )

main :: IO ()
main = do
  errors <- map solve . lines <$> inputFile

  let part1 =
        sum . map (scorePart1 . head . fst) . filter (not . null . fst) $ errors

  let part2 =
        ap (!!) ((`div` 2) . pred . length)
          . sort
          . map (foldl1 ((+) . (5 *)) . map scorePart2 . snd)
          . filter (null . fst)
          $ errors

  putStr "part 1: " >> print part1
  putStr "part 2: " >> print part2

