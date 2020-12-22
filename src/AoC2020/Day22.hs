module AoC2020.Day22 where

import           Text.ParserCombinators.ReadP
import           Data.Char
import           Control.Monad.State.Strict
import           Control.Arrow
import qualified Data.Set                      as S


data Queue a = Queue [a] [a] deriving (Show)

size :: Queue a -> Int
size (Queue a b) = length a + length b

takeQ n = toQueue . take n . toList

pop :: Queue a -> (a, Queue a)
pop (Queue []       a ) = pop (Queue (reverse a) [])
pop (Queue (a : as) bs) = (a, Queue as bs)

push :: [a] -> Queue a -> Queue a
push a (Queue as bs) = Queue as (a ++ bs)

toQueue :: [a] -> Queue a
toQueue = flip Queue []

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _             = False

toList :: Queue a -> [a]
toList (Queue a b) = a ++ reverse b


parseDeck :: ReadP [Int]
parseDeck = sepBy (read <$> munch1 isDigit) (char '\n')

parser :: ReadP ([Int], [Int])
parser = do
  string "Player 1:"
  skipSpaces
  deck1 <- parseDeck
  skipSpaces
  string "Player 2:"
  skipSpaces
  deck2 <- parseDeck
  skipSpaces
  pure (deck1, deck2)



playGame :: Queue Int -> Queue Int -> (Int, [Int])
playGame (Queue [] []) p2            = (2, toList p2)
playGame p1            (Queue [] []) = (1, toList p1)
playGame p1 p2 | d1 < d2   = playGame s1 (push [d1, d2] s2)
               | d1 > d2   = playGame (push [d2, d1] s1) s2
               | otherwise = undefined
 where
  (d1, s1) = pop p1
  (d2, s2) = pop p2

playGame2 :: S.Set ([Int], [Int]) -> (Queue Int, Queue Int) -> (Int, Queue Int)
playGame2 history (p1         , Queue [] []) = (1, p1)
playGame2 history (Queue [] [], p2         ) = (2, p2)
playGame2 history (p1, p2)
  | (toList p1, toList p2) `S.member` history
  = (1, p1)
  | size s1 >= d1 && size s2 >= d2
  = case playGame2 S.empty (takeQ d1 s1, takeQ d2 s2) of
    (1, _) -> playGame2 newHistory (push [d2, d1] s1, s2)
    (2, _) -> playGame2 newHistory (s1, push [d1, d2] s2)
  | d1 > d2
  = playGame2 newHistory (push [d2, d1] s1, s2)
  | d2 > d1
  = playGame2 newHistory (s1, push [d1, d2] s2)
  | otherwise
  = undefined
 where
  (d1, s1)   = pop p1
  (d2, s2)   = pop p2
  newHistory = (toList p1, toList p2) `S.insert` history

main :: IO ()
main = do
  (player1, player2) <- fst . head . readP_to_S (parser <* eof) <$> readFile
    "src/AoC2020/Day22.txt"

  let (winner, deck) = playGame (toQueue player1) (toQueue player2)

  putStr "part one: "
  print $ sum . zipWith (*) (reverse deck) $ [1 ..]

  let (winner2, deck2) = playGame2 S.empty (toQueue player1, toQueue player2)


  putStr "part two: "
  print $ sum . zipWith (*) (reverse . toList $ deck2) $ [1 ..]


