{-# LANGUAGE ViewPatterns #-}
module AoC2021.Day21 where

import           Data.List
import           Data.List.Split
import           Control.Monad
import           Control.Monad.State
import           Control.Arrow
import           Data.Maybe
import           System.IO.Unsafe
import           Data.Tuple
import qualified Data.Map                      as M
import           Data.Either

inputFile = readFile "src/AoC2021/Day21.txt"

playPart1 :: [Int] -> [Int] -> State ((Int, Int), (Int, Int)) Int
playPart1 p1 p2 =
  let game = zip [3, 6 ..] . concat . transpose $ [map Left p1, map Right p2]
  in  head . catMaybes <$> forM
        game
        (\(turn, play) -> case play of
          Left n -> do
            (p, score) <- gets fst
            let newP     = (p + n - 1) `mod` 10 + 1
            let newScore = score + newP
            if newScore >= 1000
              then Just . (turn *) <$> gets (snd . snd)
              else do
                modify $ first $ const (newP, newScore)
                return Nothing
          Right n -> do
            (p, score) <- gets snd
            let newP     = (p + n - 1) `mod` 10 + 1
            let newScore = score + newP
            if newScore >= 1000
              then Just . (turn *) <$> gets (snd . fst)
              else do
                modify $ second $ const (newP, newScore)
                return Nothing
        )

type P = M.Map (Int, Int) Integer
type S = State (P, P)

playPart2 :: S [Either Integer Integer]
playPart2 = do
  let x =
        ( (gets fst :: S P, \x -> modify $ first x :: S () , \x -> modify $ first $ const x :: S () , Left)
        , (gets snd :: S P, \x -> modify $ second x :: S (), \x -> modify $ second $ const x :: S (), Right)
        )
  let alternate = cycle [x, swap x]
  forM alternate $ \((p1get, p1mod, p1put, return1), (p2get, _, _, _)) -> do
    s <- M.toList <$> p1get
    p1put M.empty
    return1 . sum <$> forM
      s
      (\((pos, score), count) -> do
        let throws = map ((,) <$> head <*> const 1 . fromIntegral . length) . group . sort . map sum . replicateM 3 $ [1 .. 3]
        sum <$> forM
          throws
          (\(throw, throwCount) -> do
            let newPos    = (pos + throw - 1) `mod` 10 + 1
            let newScore  = score + newPos
            let newThrows = count * throwCount
            if newScore > 20
              then do
                other <- sum . map snd . M.toList <$> p2get
                return $ other * newThrows
              else do
                p1mod $ M.insertWith (+) (newPos, newScore) newThrows
                return 0
          )
      )

main :: IO ()
main = do
  [p1Start, p2Start] <- map (read . last . words) . lines <$> inputFile
  let x                    = p1Start :: Int

  let (p1Throws, p2Throws) = unzip . map (\[a, b] -> (a, b)) . chunksOf 2 . map sum . chunksOf 3 $ cycle [1 .. 100]

  let part1 = evalState (playPart1 p1Throws p2Throws) ((p1Start, 0), (p2Start, 0))

  let (sum . take 21 -> p1Wins, sum . take 21 -> p2Wins) =
        partitionEithers $ evalState playPart2 (M.singleton (p1Start, 0) 1, M.singleton (p2Start, 0) 1)

  putStr "part 1: " >> print part1
  putStr "part 2: " >> print (max p1Wins p2Wins)

