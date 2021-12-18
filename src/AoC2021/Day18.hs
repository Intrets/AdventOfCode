{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module AoC2021.Day18 where

import           Control.Monad
import           Data.List
import           Text.ParserCombinators.ReadP
                                         hiding ( get )
import           Data.Char
import           Control.Applicative
import           Data.Maybe
import           Control.Monad.State
import           Data.Either

whileM :: (Monad m) => m (Maybe b) -> m [b]
whileM a = do
  r <- a
  case r of
    Just r0 -> (r0 :) <$> whileM a
    Nothing -> return []

inputFile = readFile "src/AoC2021/Day18.txt"

data SnailNumber = Number Int | Pair SnailNumber SnailNumber deriving (Show, Eq)

magnitude (Number n) = n
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

pprint x = pprint' x >> putStrLn ""

pprint' :: SnailNumber -> IO ()
pprint' (Number n) = putStr $ show n
pprint' (Pair l r) =
  putStr "[" >> pprint' l >> putStr "," >> pprint' r >> putStr "]"

toTuple :: SnailNumber -> (Int, Int)
toTuple (Number _                  ) = undefined
toTuple (Pair (Number a) (Number b)) = (a, b)
toTuple _                            = undefined

data Trace = TLeft | TRight deriving (Show, Eq)

maybeHead []      = Nothing
maybeHead (a : _) = Just a

number :: ReadP SnailNumber
number = Number . read <$> munch isDigit

pair :: ReadP SnailNumber
pair = do
  char '['
  s1 <- parse
  char ','
  s2 <- parse
  char ']'
  return $ Pair s1 s2

parse :: ReadP SnailNumber
parse = number <|> pair

findExplode :: SnailNumber -> Maybe [Trace]
findExplode = findExplode' 0
 where
  findExplode' level = \case
    Number _                   -> Nothing
    Pair (Number _) (Number _) -> if level >= 4 then Just [] else Nothing
    Pair left right ->
      maybeHead
        . catMaybes
        $ [ (TLeft :) <$> findExplode' (level + 1) left
          , (TRight :) <$> findExplode' (level + 1) right
          ]

explodeNumber :: [Trace] -> SnailNumber -> ((Int, Int), SnailNumber)
explodeNumber []           number     = (toTuple number, Number 0)
explodeNumber (dir : rest) (Pair l r) = case dir of
  TLeft  -> let (n, s) = explodeNumber rest l in (n, Pair s r)
  TRight -> let (n, s) = explodeNumber rest r in (n, Pair l s)

apply :: (Int -> Int) -> SnailNumber -> [Trace] -> SnailNumber
apply f (Number n) _               = Number (f n)
apply f (Pair l r) (TLeft  : rest) = Pair (apply f l rest) r
apply f (Pair l r) (TRight : rest) = Pair l (apply f r rest)

applyExplode :: SnailNumber -> [Trace] -> SnailNumber
applyExplode number trace = number

dropOne f [] = []
dropOne f (a : rest) | f a       = rest
                     | otherwise = a : rest

trimRight (TLeft  : rest) = TRight : rest
trimRight (TRight : rest) = (TRight :) . drop 1 . dropWhile (== TRight) $ rest

trimLeft (TRight : rest) = TLeft : rest
trimLeft (TLeft  : rest) = (TLeft :) . drop 1 . dropWhile (== TLeft) $ rest


addToRight :: Int -> [Trace] -> SnailNumber -> SnailNumber
addToRight n trace number | null t    = number
                          | otherwise = apply (+ n) number (t ++ repeat TLeft)
  where t = reverse . trimRight . reverse $ trace

addToLeft :: Int -> [Trace] -> SnailNumber -> SnailNumber
addToLeft n trace number | null t    = number
                         | otherwise = apply (+ n) number (t ++ repeat TRight)
  where t = reverse . trimLeft . reverse $ trace

doSplit :: SnailNumber -> Either SnailNumber SnailNumber
doSplit number@(Number n)
  | n >= 10   = Left $ Pair (Number $ n `div` 2) (Number $ n - n `div` 2)
  | otherwise = Right number
doSplit pair@(Pair l r) =
  let l0 = doSplit l
      r0 = doSplit r
  in  if
        | isLeft l0 -> Left $ Pair (fromLeft undefined l0) r
        | isLeft r0 -> Left $ Pair l (fromLeft undefined r0)
        | otherwise -> Right pair

doSplitState :: State SnailNumber Bool
doSplitState = state $ \s -> case doSplit s of
  Left  s1 -> (True, s1)
  Right _  -> (False, s)

--solve :: State SnailNumber SnailNumber
solve s =
  flip evalState s
    $   last
    .   catMaybes
    .   takeWhile isJust
    .   (Just s :)
    <$> replicateM
          100000
          (do
            exploded <- fmap (not . null) . whileM $ do
              explode <- gets findExplode
              case explode of
                Nothing   -> return Nothing
                Just path -> do
                  (l, r) <- state $ explodeNumber path
                  unless (all (== TRight) path) $ modify $ addToRight r path
                  unless (all (== TLeft) path) $ modify $ addToLeft l path
                  return $ Just True

            splitted <- doSplitState

            if splitted || exploded then gets Just else return Nothing
          )

main :: IO ()
main = do
  numbers <- map (fst . head . readP_to_S (parse <* eof)) . lines <$> inputFile

  let part1 = foldl1 (\acc e -> solve $ Pair acc e) numbers
  print $ magnitude part1

  let part2 =
        maximum $ [ magnitude . solve $ Pair x y | x <- numbers, y <- numbers, x /= y ]
  print part2

  putStr "part 1: " >> print part1 
  putStr "part 2: " >> print part2

