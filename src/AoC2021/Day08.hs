module AoC2021.Day08 where

import           Data.List.Split
import           Control.Monad.State
import           Data.List               hiding ( union
                                                , (\\)
                                                )
import qualified Data.Map                      as M
import           Data.Set                       ( intersection
                                                , (\\)
                                                , size
                                                , union
                                                , isSubsetOf
                                                )
import qualified Data.Set                      as S
import           Control.Arrow

(.:) = (.) . (.)
infixr 8 .:

inputFile = readFile "src/AoC2021/Day08.txt"

type Digit = S.Set Char

extract :: (a -> Bool) -> S.Set a -> (a, S.Set a)
extract f = first (S.elemAt 0) . S.partition f

lengthMatch :: Int -> State (S.Set Digit) Digit
lengthMatch n = state $ extract ((== n) . size)

match :: Digit -> State (S.Set Digit) Digit
match str = state $ extract (str `isSubsetOf`)

getDecodeMap :: S.Set Digit -> M.Map Digit Int
getDecodeMap = evalState $ do
  one   <- lengthMatch 2
  four  <- lengthMatch 4
  seven <- lengthMatch 3
  eight <- lengthMatch 7

  let ninePart = four `union` seven
  nine <- match ninePart

  let sixPart = eight \\ seven
  six <- match sixPart

  let zeroPart = (six \\ four) `union` seven
  zero <- match zeroPart

  let twoPart = six \\ four
  two   <- match twoPart

  three <- match seven

  five  <- match S.empty

  return $ M.fromList $ zip
    [zero, one, two, three, four, five, six, seven, eight, nine]
    [0 ..]

solvePart2 :: [Digit] -> Int
solvePart2 digits =
  let decode = (M.!) (getDecodeMap (S.fromList digits))
  in  foldl1 ((+) . (10 *)) . map decode . reverse . take 4 . reverse $ digits

main :: IO ()
main = do

  part1 <-
    length
    .   filter (`elem` [2, 3, 4, 7])
    .   map length
    .   concatMap (last . splitOn ["|"] . words)
    .   lines
    <$> inputFile

  part2 <-
    sum
    .   map (solvePart2 . (map S.fromList . filter (/= "|") . words))
    .   lines
    <$> inputFile

  putStr "part 1: " >> print part1
  putStr "part 2: " >> print part2

