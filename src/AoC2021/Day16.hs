module AoC2021.Day16 where

import           Data.List
import qualified Data.Map                      as M
import           Data.Char
import           Data.Bool
import           Control.Monad
import           Text.ParserCombinators.ReadP
import           Control.Applicative     hiding ( many )

inputFile = readFile "src/AoC2021/Day16.txt"

readBinary :: String -> Integer
readBinary = foldl ((. fromIntegral . digitToInt) . (+) . (* 2)) 0

readBits bits = readBinary <$> count bits get

data Operator = Literal {
  version :: Integer,
  value :: Integer
} | Other {
  version :: Integer,
  id :: Integer,
  sub :: [Operator]
} deriving (Show)

getVersion (Literal v _) = v
getVersion (Other v _ _) = v

foldOp :: (a -> Operator -> a) -> a -> Operator -> a
foldOp f a li@Literal{}       = f a li
foldOp f a op@(Other _ _ sub) = foldl (foldOp f) (f a op) sub

eval :: Operator -> Integer
eval (Literal _ v) = v
eval (Other _ id vs) =
  let op = case id of
        0 -> sum
        1 -> product
        2 -> minimum
        3 -> maximum
        5 -> \[a, b] -> if a > b then 1 else 0
        6 -> \[a, b] -> if a < b then 1 else 0
        7 -> \[a, b] -> if a == b then 1 else 0
  in  op $ map eval vs

parsePacket :: ReadP Operator
parsePacket = do
  version <- readBits 3
  id      <- readBits 3
  case id of
    4 -> do
      lead <- many (char '1' >> count 4 get)
      end  <- char '0' >> count 4 get
      return $ Literal version (readBinary $ concat lead ++ end)
    _ -> do
      lengthTypeID <- readBits 1
      subPackets   <- case lengthTypeID of
        0 -> do
          bitLength <- readBits 15
          bits      <- count (fromInteger bitLength) get
          return $ fst . last . readP_to_S (many parsePacket) $ bits
        1 -> do
          packets <- readBits 11
          count (fromInteger packets) parsePacket
      return $ Other version id subPackets

main :: IO ()
main = do
  let decode = (M.!) . M.fromList $ zip
        (map intToDigit [0 .. 9] ++ ['A' .. 'F'])
        (replicateM 4 ['0', '1'])

  decoded <- concatMap decode . takeWhile isAlphaNum <$> inputFile

  let ops = fst . last . readP_to_S parsePacket $ decoded
  let part1 = foldOp ((. getVersion) . (+)) 0 ops
  let part2 = eval ops

  putStr "part 1: " >> print part1
  putStr "part 2: " >> print part2

