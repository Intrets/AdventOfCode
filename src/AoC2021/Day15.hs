module AoC2021.Day15 where

import           Data.List
import qualified Data.HashMap.Strict           as M
import           Control.Monad.State
import qualified Data.Set                      as S
import           Control.Arrow
import           Data.Bifunctor                 ( bimap )
import           Data.Ord
import qualified Data.Heap                     as H
import           Data.List.Split
import           Data.STRef.Strict

inputFile = readFile "src/AoC2021/Day15.txt"

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ f a = do
  b <- f
  when b $ a >> whileM_ f a

pop :: S (Vec2, Integer)
pop = do
  Just (H.Entry v k, m) <- gets $ H.viewMin . tfst
  modify $ tfirst $ const m
  return (k, v)
--   modify $ first $ const m
--   return k

-- shift :: Vec2 -> S ()
-- shift v = do
--   value <- gets $ (M.! v) . fst
--   modify $ bimap (M.delete v) (M.insert v value)

type Vec2 = (Integer, Integer)
data Triple = Triple {
  tfst :: !(H.Heap (H.Entry Integer Vec2)),
  tsnd :: !(S.Set Vec2),
  tthd :: !(M.HashMap Vec2 Integer)
} deriving (Show)

tfirst f (Triple a b c) = Triple (f a) b c
tsecond f (Triple a b c) = Triple a (f b) c
tthird f (Triple a b c) = Triple a b (f c)

type S a = State Triple a

solve :: (Vec2 -> Maybe Integer) -> S ()
solve getWeight = do
  whileM_ (gets $ not . H.null . tfst) $ do
    Just (H.Entry value point, m) <- gets $ H.viewMin . tfst
    modify $ tfirst $ const m

    unvisited <- gets $ S.member point . tsnd

    when unvisited $ do
      let neighbours = map ($point) ([first, second] <*> [succ, pred])

      forM_ neighbours $ \v -> do
        unvisited <- gets $ S.member v . tsnd
        when unvisited $ case getWeight v of
          Just weight -> do
            let newValue = value + weight
            let action   = M.insertWith min v newValue
            modify $ tthird action
            modify $ tfirst $ H.insert $ H.Entry newValue v
          Nothing -> return ()
      modify $ tsecond $ S.delete point
    --modify $ bimap (M.delete point) (M.insert point value)

  return ()

main :: IO ()
main = do
  weights <- map (map (read . pure)) . lines <$> inputFile
  -- let weights = replicate 2 weights_
  let width  = fromIntegral . length . head $ weights
  let height = fromIntegral . length $ weights

  print (width, height)

  let points    = (,) <$> [0 .. width - 1] <*> [0 .. height - 1]

  let getWeight = flip M.lookup . M.fromList $ zip points (concat weights)

  let unvisited = S.fromList points

  let part1 = tthd . execState (solve getWeight) $ Triple
        (H.singleton (H.Entry 0 (0, 0)))
        unvisited
        M.empty

  let pairs = zip points (concat weights)
  let part2Weights =
        concatMap
            (\(x, y) -> map
              (bimap (bimap (+ x * width) (+ y * height))
                     (succ . (`mod` 9) . pred . (+ (x + y)))
              )
              pairs
            )
          $   (,)
          <$> [0 .. 4]
          <*> [0 .. 4]

  let getWeight2 = flip M.lookup . M.fromList $ part2Weights

  let points2 = (,) <$> [0 .. width * 5 - 1] <*> [0 .. height * 5 - 1]
  let unvisited2 = S.fromList points2
  let part2 = tthd $ execState (solve getWeight2) $ Triple
        (H.singleton (H.Entry 0 (0, 0)))
        unvisited2
        M.empty

  -- print part2Weights

  putStr "part 1: " >> print (part1 M.! (width - 1, height - 1))
--   print part1
  putStr "part 2: " >> print (part2 M.! (width * 5 - 1, height * 5 - 1))


