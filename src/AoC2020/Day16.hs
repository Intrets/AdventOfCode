{-# LANGUAGE TupleSections #-}
module AoC2020.Day16 where

import           Text.ParserCombinators.ReadP
import           Control.Applicative
import           Control.Monad
import           Data.Char
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector                   as V
import           Data.List
import qualified Data.IntSet                   as IS
import           Control.Arrow

parseInt :: ReadP Int
parseInt = read <$> munch1 isDigit

parser :: ReadP ([(String, [(Int, Int)])], [Int], [[Int]])
parser = do
  fields <- sepBy1
    (do
      name <- munch1 (/= ':')
      void get >> void get
      ranges <- sepBy1 ((,) <$> parseInt <* char '-' <*> parseInt)
                       (string " or ")
      pure (name, ranges)
    )
    (char '\n')
  skipSpaces
  string "your ticket:"
  skipSpaces
  yourTicket <- sepBy1 parseInt (char ',')
  skipSpaces
  string "nearby tickets:"
  skipSpaces
  nearbyTickets <- many1 (sepBy1 parseInt (char ',') <* char '\n')

  pure (fields, yourTicket, nearbyTickets)


main :: IO ()
main = do
  inputData <- readFile "src/AoC2020/Day16.txt"

  let (fields, yourTicket, nearbyTickets) =
        fst . last $ readP_to_S (parser <* eof) inputData

  let invalid = (VU.!)
        (     VU.replicate 1000 True
        VU.// ( map (, False)
              . concatMap (uncurry enumFromTo)
              . concatMap snd
              $ fields
              )
        )

  putStr "part one: "
  print $ sum . filter invalid . concat $ nearbyTickets

  putStr "part two: "
  let validTickets = filter (not . any invalid) (yourTicket : nearbyTickets)

  let valid2 = (V.!)
        ( V.fromList
        $ map ((\l x -> any (\(a, b) -> a <= x && x <= b) l) . snd) fields
        )

  let getValids number =
        IS.fromList $ filter (`valid2` number) [0 .. length fields - 1]

  let d =
        (   (:)
          <$> head
          <*> ap (zipWith (\(i1, e1) (i2, e2) -> (i2, IS.difference e2 e1)))
                 tail
          )
          $ sortOn (IS.size . snd)
          . zip [0 ..]
          . map (foldl1 IS.intersection . map getValids)
          . transpose
          $ validTickets

  let e = map fst . sortOn snd . map (second $ head . IS.toList) $ d

  let d = product . map (yourTicket !!) . take 6 $ e

  print d










