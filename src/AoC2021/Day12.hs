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

isSmall (Small _) = True
isSmall _         = False

type S a = State ([Cave], S.Set Cave) a
emptyS = ([], S.empty)

isVisited :: Cave -> S Bool
isVisited cave = (isSmall cave &&) <$> gets (S.member cave . snd)

visit :: Cave -> S ()
visit cave = modify $ bimap (cave :) (S.insert cave)

popVisit :: S ()
popVisit = do
  top <- gets (head . fst)
  modify $ first tail
  modify $ second (S.delete top)

solve :: Bool -> (Cave -> [Cave]) -> Int
solve bypass getNeighbours = evalState (search bypass (Small "start")) emptyS
 where
  search _      (Small "end") = return 1
  search bypass cave          = do
    visited <- isVisited cave
    if
      | visited && (cave == Small "start" || not bypass) -> return 0
      | not visited -> do
        visit cave
        total <- sum <$> mapM (search bypass) (getNeighbours cave)
        popVisit
        return total
      | otherwise -> sum <$> mapM (search False) (getNeighbours cave)

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

  putStr "part 1: " >> print (solve False graph)
  putStr "part 2: " >> print (solve True graph)

