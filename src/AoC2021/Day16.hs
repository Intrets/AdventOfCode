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

data Operator = Literal {
  version :: Integer,
  value :: Integer
} | Operator {
  version :: Integer,
  id :: Integer,
  sub :: [Operator]
} deriving (Show)


opprint = pprint 0
 where
  pprint :: Int -> Operator -> IO ()
  pprint level (Literal _ value) =
    putStr (replicate (2 * level) ' ') >> putStrLn ("Literal " ++ show value)
  pprint level (Operator version id sub) = do
    putStr (replicate (2 * level) ' ')
    putStrLn $ "Operator " ++ op ++ " {"
    mapM_ (pprint $ level + 1) sub
    putStr (replicate (2 * level) ' ')
    putStrLn "}"
   where
    op = case id of
      0 -> "sum"
      1 -> "prod"
      2 -> "min"
      3 -> "max"
      5 -> "gt"
      6 -> "lt"
      7 -> "eq"

foldOp :: (a -> Operator -> a) -> a -> Operator -> a
foldOp f a li@Literal{}          = f a li
foldOp f a op@(Operator _ _ sub) = foldl (foldOp f) (f a op) sub

eval :: Operator -> Integer
eval (Literal _ v) = v
eval (Operator _ id vs) =
  let op = case id of
        0 -> sum
        1 -> product
        2 -> minimum
        3 -> maximum
        5 -> \[a, b] -> bool 0 1 (a > b)
        6 -> \[a, b] -> bool 0 1 (a < b)
        7 -> \[a, b] -> bool 0 1 (a == b)
  in  op $ map eval vs

parsePacket :: ReadP Operator
parsePacket = do
  version <- readBinary <$> count 3 get
  id      <- readBinary <$> count 3 get
  case id of
    4 -> do
      lead <- many (char '1' >> count 4 get)
      end  <- char '0' >> count 4 get
      return $ Literal version $ readBinary $ concat lead ++ end
    _ -> do
      lengthTypeID <- readBinary <$> count 1 get
      subPackets   <- case lengthTypeID of
        0 -> do
          bitLength <- readBinary <$> count 15 get
          bits      <- count (fromInteger bitLength) get
          return $ fst . last . readP_to_S (many parsePacket) $ bits
        1 -> do
          packets <- readBinary <$> count 11 get
          count (fromInteger packets) parsePacket
      return $ Operator version id subPackets

main :: IO ()
main = do
  let decode = (M.!) . M.fromList $ zip
        (map intToDigit [0 .. 9] ++ ['A' .. 'F'])
        (replicateM 4 ['0', '1'])

  decoded <- concatMap decode . takeWhile isAlphaNum <$> inputFile

  let ops = fst . last . readP_to_S parsePacket $ decoded
  let part1 = foldOp
        (\acc e -> case e of
          Literal version _    -> version + acc
          Operator version _ _ -> version + acc
        )
        0
        ops
  let part2 = eval ops

  -- opprint ops

  putStr "part 1: " >> print part1
  putStr "part 2: " >> print part2

