{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module AoC2020.Day20 where

import           Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadP  as P
                                                ( get )
import           Data.Char
import           Control.Monad
import           Data.List
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Control.Monad.State
import           Data.Maybe
import qualified Data.Vector.Unboxed           as V
import           Data.Function

reverseBits = (V.!)
  (    V.replicate 1024 0
  V.// ( ap (zipWith ((,) `on` foldl1 (\acc e -> acc * 2 + e))) (map reverse)
       . replicateM 10
       $ [0, 1]
       )
  )

data Tile = Tile {up :: Int, right :: Int, down :: Int, left :: Int} deriving (Eq, Ord)

instance Show Tile where
  show (Tile a b c d) =
    "Tile (" ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ ")"

rotateTile (Tile a b c d) = Tile b c d a
flipTile (Tile a b c d) =
  Tile (reverseBits c) (reverseBits b) (reverseBits a) (reverseBits d)

getOrientations tile =
  (take 4 . iterate rotateTile $ tile)
    ++ (take 4 . iterate rotateTile . flipTile $ tile)

parser :: ReadP [(Int, [String])]
parser = sepBy1
  (do
    string "Tile "
    n <- (read :: String -> Int) <$> munch1 isDigit
    string ":\n"
    s <- count 10 (count 10 P.get <* char '\n')
    pure (n, s)
  )
  (char '\n')

valid :: M.Map (Int, Int) Tile -> (Int, Int) -> Tile -> [Tile]
valid state (x, y) = filter validTile . getOrientations
 where
  validTile t = and $ mapMaybe
    (\(pos, validator) -> ($) validator <$> (state M.!? pos))
    [ ((x, y + 1), (==) (up t) . down)
    , ((x, y - 1), (==) (down t) . up)
    , ((x + 1, y), (==) (right t) . left)
    , ((x - 1, y), (==) (left t) . right)
    ]

everyOne :: [a] -> [(a, [a])]
everyOne a = everyOne' a []

everyOne' []       _  = []
everyOne' (a : as) bs = (a, as ++ bs) : everyOne' as (a : bs)

solve
  :: [Tile]
  -> [(Int, Int)]
  -> M.Map (Int, Int) Tile
  -> Maybe (M.Map (Int, Int) Tile)
solve _     []             state = Just state
solve tiles (next : order) state = msum $ map
  (\(tile, (notPicked++) -> others) ->
    solve others order (M.insert next tile state)
  )
  pairs
 where
  a                              = map (ap (,) (valid state next)) tiles
  (picked, map fst -> notPicked) = partition (not . null . snd) a
  pairs =
    concatMap (\((_, p), map fst -> ps) -> map (, ps) p) $ everyOne picked

makeOrder n = concatMap
  (\i -> map (\j -> (j, i - j)) [max (i - n + 1) 0 .. (min (n - 1) i)])
  [0 .. 2 * n - 1]

squareToTile :: [String] -> Tile
squareToTile s = Tile (getNumber up)
                      (getNumber right)
                      (getNumber down)
                      (getNumber left)
 where
  left  = map head s
  right = map last s
  up    = head s
  down  = last s
  getNumber =
    foldl1 (\acc e -> acc * 2 + e) . map (\c -> if c == '#' then 1 else 0)

main :: IO ()
main = do
  inputData <- fst . head . readP_to_S (parser <* eof) <$> readFile
    "src/AoC2020/Day20.txt"

  let squares = map snd inputData

  let edges1  = map head squares
  let edges2  = map last squares
  let edges3  = map (map head) squares
  let edges4  = map (map last) squares

  let edges5  = edges1 ++ edges2 ++ edges3 ++ edges4
  let edges   = edges5 ++ map reverse edges5

  print $ length . filter (== 1) . map length . group . sort $ edges

  let test = map squareToTile squares

  let state :: M.Map (Int, Int) Tile
      state = M.fromList [((0, -1), Tile 4 4 4 4), ((-1, 0), Tile 3 3 3 3)]

  let run = solve test (makeOrder 3) M.empty

  print $ run

  putStrLn "day20"
