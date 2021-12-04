{-# LANGUAGE FlexibleContexts #-}
module AoC2021.Day04 where

import           Data.List.Split
import           Data.List
import qualified Data.IntSet                   as IS
import           Control.Monad.State
import           Control.Monad
import           Control.Arrow

inputFile = readFile "src/AoC2021/Day04.txt"

main :: IO ()
main = do
  (numbers_ : boards_) <- filter (not . null) . lines <$> inputFile

  let readInt = read :: String -> Int
  let numbers = map readInt . splitOn "," $ numbers_
  let boards =
        map
            ( ((,) <*> (map IS.fromList . ((++) <*> transpose)))
            . map (map readInt . words)
            )
          . chunksOf 5
          $ boards_

  let game = flip evalState boards
        $ forM
            numbers
            (state . (((,) <*> id) .) . map . second . map . IS.delete)
  let wat =
        zip (concat game) (tail (inits numbers) >>= replicate (length boards))

  let Just ((board1, _), draws1) = find (any IS.null . snd . fst) wat
  let unmarked1                  = filter (`notElem` draws1) . concat $ board1

  putStr "part 1: " >> print (last draws1 * sum unmarked1)

  let wat2 = zip
        (concat game)
        ((tail . tail $ inits numbers) >>= replicate (length boards))
  let ((board2, _), draws2) =
        last $ filter (not . any IS.null . snd . fst) wat2
  let unmarked2 = filter (`notElem` draws2) . concat $ board2

  putStr "part 2: " >> print (last draws2 * sum unmarked2)

