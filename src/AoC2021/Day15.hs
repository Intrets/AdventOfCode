module AoC2021.Day15 where

import           Data.List
import qualified Data.Map                      as M
import           Control.Monad.State
import qualified Data.Set                      as S
import           Control.Arrow
import           Data.Bifunctor                 ( bimap )
import           Data.Ord
import qualified Data.Heap                     as H

inputFile = readFile "src/AoC2021/Day15.txt"

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ f a = do
  b <- f
  when b $ a >> whileM_ f a

pop :: S (Vec2, Integer)
pop = do
  (k, m) <- gets $ M.deleteFindMin . fst
  modify $ first $ const m
  return k

shift :: Vec2 -> S ()
shift v = do
  value <- gets $ (M.! v) . fst
  modify $ bimap (M.delete v) (M.insert v value)

type Vec2 = (Integer, Integer)
type S a = State (M.Map Vec2 Integer, M.Map Vec2 Integer) a

solve :: (Vec2 -> Maybe Integer) -> S ()
solve getWeight = do
  whileM_ (gets $ not . M.null . fst) $ do
    (point, value) <- gets $ minimumBy (comparing snd) . M.toList . fst

    let neighbours = map ($point) ([first, second] <*> [succ, pred])

    forM_ neighbours $ \v -> do
      unvisited <- gets $ M.member v . fst
      when unvisited $ case getWeight v of
        Just weight -> do
          let newValue = value + weight
          let action   = M.insertWith min v newValue
          modify $ first action
        Nothing -> return ()

    modify $ bimap (M.delete point) (M.insert point value)

  return ()

main :: IO ()
main = do
  weights <- map (map (read . pure)) . lines <$> inputFile
  let width     = fromIntegral . length . head $ weights
  let height    = fromIntegral . length $ weights

  let points    = (,) <$> [0 .. width - 1] <*> [0 .. height - 1]

  let getWeight = flip M.lookup . M.fromList $ zip points (concat weights)

  let unvisited = M.insert (0, 0) 0 . M.fromList . zip points $ repeat 10000000

  let part1     = snd $ execState (solve getWeight) (unvisited, M.empty)

  let pairs     = zip points (concat weights)
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

  let points2    = (,) <$> [0 .. width * 5 - 1] <*> [0 .. height * 5 - 1]
  let unvisited2 =
        M.insert (0, 0) 0 . M.fromList . zip points2 $ repeat 10000000
  let part2 = snd $ execState (solve getWeight2) (unvisited2, M.empty)

  -- print part2Weights

  putStr "part 1: " >> print (part1 M.! (width - 1, height - 1))
  putStr "part 2: " >> print (part2 M.! (width * 5 - 1, height * 5 - 1))


