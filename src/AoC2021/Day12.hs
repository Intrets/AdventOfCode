{-# LANGUAGE MultiWayIf #-}
module AoC2021.Day12 where

import           Data.List.Split
import qualified Data.Map                      as M
import           Control.Monad.State
import           Control.Monad
import           Control.Applicative
import qualified Data.Set                      as S
import           Data.Char
import           Data.List
import           Control.Arrow
import           Data.Bifunctor                 ( bimap )

inputFile = readFile "src/AoC2021/Day12.txt"

data Cave = Small String | Large String deriving (Ord, Eq, Show, Read)

start = Small "start"
end = Small "end"

isSmall (Small _) = True
isSmall _         = False

type S a = State ([Cave], S.Set Cave) a
emptyS = ([], S.empty)

isVisited :: Cave -> S Bool
isVisited cave = (isSmall cave &&) <$> gets (S.member cave . snd)

visit :: Cave -> S ()
visit cave = modify $ bimap (cave :) (S.insert cave)

pop :: S ()
pop = do
  top <- gets (head . fst)
  modify $ first tail
  modify $ second (S.delete top)


solve :: (Cave -> [Cave]) -> State ([Cave], S.Set Cave) Int
solve getNeighbours = do

  let search _      (Small "end") = return 1
      search bypass cave          = do
        visited <- isVisited cave
        if
          | visited && (cave == start || not bypass) -> return 0
          | not visited -> do
            visit cave
            res <- sum <$> mapM (search bypass) (getNeighbours cave)
            pop
            return res
          | otherwise -> sum <$> mapM (search False) (getNeighbours cave)

  search True start


main :: IO ()
main = do
  graph <-
    (M.!)
    .   M.unionsWith (++)
    .   map (M.fromList . pure)
    .   concatMap
          ( (\[a, b] -> [(a, [b]), (b, [a])])
          . map (\s -> if all isUpper s then Large s else Small s)
          . splitOn "-"
          )
    .   lines
    <$> inputFile

  let part1 = runState (solve graph) emptyS

  print part1

  -- print graph

  putStr "part 1: " >> print ""
  putStr "part 2: " >> print ""

