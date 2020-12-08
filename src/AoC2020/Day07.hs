module AoC2020.Day07 where

import qualified Data.IntSet                   as S
import qualified Data.IntMap                   as IM
import qualified Data.Map                      as M
import qualified Data.Vector                   as V
import           Text.ParserCombinators.ReadP
import           Control.Arrow
import           Control.Monad.State           as State
import           Data.Tuple
import           Data.Char

whileM_ :: (Monad m) => m (Maybe c) -> m b -> m c
whileM_ b a = do
  r <- b
  case r of
    Just c  -> pure c
    Nothing -> a >> whileM_ b a

colorP :: ReadP String
colorP = (++) <$> munch1 isLetter <* skipSpaces <*> munch1 isLetter

colorP2 :: ReadP (Int, String)
colorP2 = do
  n <- read <$> munch1 isDigit
  skipSpaces
  bag <- (++) <$> munch1 isLetter <* skipSpaces <*> munch1 isLetter
  if n == 1 then string " bag" else string " bags"
  pure (n, bag)

parser1 :: ReadP (String, [(Int, String)])
parser1 = do
  color <- colorP <* string " bags contain "
  bags  <- sepBy1 colorP2 (string ", ") <++ (string "no other bags" >> pure [])
  char '.'
  pure (color, bags)

solve1 :: V.Vector [Int] -> State (S.IntSet, [Int]) Int
solve1 graph = pred <$> whileM_
  (do
    q <- gets snd
    if null q then Just . S.size <$> gets fst else pure Nothing
  )
  (do
    (set, q) <- state $ (,) <$> second head <*> second tail
    unless (q `S.member` set) $ modify $ S.insert q *** ((graph V.! q) ++)
  )

solve2 :: V.Vector [(Int, Int)] -> Int -> State (IM.IntMap Int) Int
solve2 graph n = do
  b <- gets (IM.member n)
  if b
    then gets (IM.! n)
    else do
      ans <- sum
        <$> mapM (\(c, e) -> (* c) . succ <$> solve2 graph e) (graph V.! n)
      modify (IM.insert n ans)
      pure ans

main = do
  inputData <- map (fst . head . readP_to_S parser1) . lines <$> readFile
    "src/AoC2020/Day07.txt"

  let colors  = map fst inputData
  let toIndex = (M.!) (M.fromList (zip colors [0 ..]))
  let toColor = (V.!) (V.fromList colors)

  let pairs = concatMap
        (uncurry zip . ((repeat . toIndex) *** map (second toIndex)))
        inputData

  let graph1 =
        V.accum (flip (:)) (V.replicate (length colors) [])
          . map (swap . second snd)
          $ pairs

  putStr "part one: "
  print . flip evalState (S.empty, [toIndex "shinygold"]) $ solve1 graph1

  let graph2 = V.accum (flip (:)) (V.replicate (length colors) []) pairs

  putStr "part two: "
  print . flip evalState IM.empty $ solve2 graph2 (toIndex "shinygold")
