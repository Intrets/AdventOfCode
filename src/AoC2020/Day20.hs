{-# LANGUAGE TupleSections #-}
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
import           Control.Arrow                  ( (***)
                                                , first
                                                )
import           Data.List.Split
import qualified Data.Array.Unboxed            as A
import           Control.Monad.ST.Strict
import qualified Data.Array.ST                 as AS


reverseBits = V.unsafeIndex
  (    V.replicate 1024 0
  V.// ( ap (zipWith ((,) `on` fromBinary)) (map reverse)
       . replicateM 10
       $ [0, 1]
       )
  )

data Tile = Tile {ops :: String, raw :: [String], index :: Int, up :: !Int, right :: !Int, down :: !Int, left :: !Int} deriving (Eq, Ord)

transformRaw :: String -> [String] -> [String]
transformRaw ops = transformRaw' (reverse ops)

transformRaw' :: String -> [String] -> [String]
transformRaw' []           = id
transformRaw' ('r' : rest) = transformRaw' rest . map reverse . transpose
transformRaw' ('f' : rest) = transformRaw' rest . map reverse

rotateTile (Tile ops raw i a b c d) =
  Tile ('r' : ops) raw i (reverseBits d) a (reverseBits b) c
flipTile (Tile ops raw i a b c d) =
  Tile ('f' : ops) raw i (reverseBits a) d (reverseBits c) b

getOrientations :: Tile -> [Tile]
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

validTile :: (Int, Int) -> M.Map (Int, Int) Tile -> Tile -> Bool
validTile (x, y) state t = and $ mapMaybe
  (\(pos, validator) -> ($) validator <$> (state M.!? pos))
  [ ((x, y + 1), (==) (up t) . down)
  , ((x, y - 1), (==) (down t) . up)
  , ((x + 1, y), (==) (right t) . left)
  , ((x - 1, y), (==) (left t) . right)
  ]

everyOne :: [a] -> [(a, [a])]
everyOne a = everyOne' a []
 where
  everyOne' []       _  = []
  everyOne' (a : as) bs = (a, as ++ bs) : everyOne' as (a : bs)

makeOrder :: Int -> [(Int, Int)]
makeOrder n = concatMap
  (\i -> map (\j -> (j, i - j)) [max (i - n + 1) 0 .. (min (n - 1) i)])
  [0 .. 2 * n - 1]

squareToTile :: (Int, [String]) -> Tile
squareToTile (i, s) = Tile []
                           s
                           i
                           (getNumber up)
                           (getNumber right)
                           (getNumber down)
                           (getNumber left)
 where
  left      = map head s
  right     = map last s
  up        = head s
  down      = last s
  getNumber = fromBinary . map (\c -> if c == '#' then 1 else 0)

solve
  :: M.Map (Int, Int) Tile
  -> [(Int, Int)]
  -> [Tile]
  -> Maybe (M.Map (Int, Int) Tile)
solve state _             []    = Just state
solve state []            _     = Just state
solve state (pos : order) tiles = msum
  $ map (\(t, others) -> solve (M.insert pos t state) order others) valids
 where
  possibilities =
    concatMap (\(t, others) -> map (, others) $ getOrientations t)
      $ everyOne tiles
  valids = filter (\(t, others) -> validTile pos state t) possibilities

main :: IO ()
main = do
  inputData <-
    map squareToTile . fst . head . readP_to_S (parser <* eof) <$> readFile
      "src/AoC2020/Day20.txt"

  let order    = round . sqrt . fromIntegral . length $ inputData

  let Just run = solve M.empty (makeOrder order) inputData

  let ans =
        product
          . map (\[a, b] -> fromIntegral . (index . (run M.!)) $ (a, b))
          $ sequence [[order - 1, 0], [order - 1, 0]]
  putStr "part one: "
  print ans

  let canvas =
        concatMap (foldl1 (zipWith (++)))
          . transpose
          . chunksOf order
          . map
              ( reverse
              . (\(Tile ops raw i _ _ _ _) ->
                  map (init . tail) . init . tail . transformRaw ops $ raw
                )
              )
          . M.elems
          $ run

  let
    monster =
      map fst
        . filter ((== '#') . snd)
        . concat
        . zipWith (\i -> zipWith (\j -> ((i, j), )) [0 ..]) [0 ..]
        $ [ "                  # "
          , "#    ##    ##    ###"
          , " #  #  #  #  #  #   "
          ]

  let monsterChecks = map (\[a, b] -> (a, b))
        $ sequence [[0 .. 8 * order - 3], [0 .. 8 * order - 20]]

  let aaaa c = checkMonster (8 * order - 1, 8 * order - 1)
                            (concat c)
                            monster
                            monsterChecks
  let trans = map (`transformRaw` canvas)
                  ["", "r", "rr", "rrr", "f", "fr", "frr", "frrr"]
  let bbbb = minimum $ map aaaa trans

  putStr "part two: "
  print bbbb

checkMonster :: (Int, Int) -> String -> [(Int, Int)] -> [(Int, Int)] -> Int
checkMonster bounds canvas dragon offset = total - dragonSpots
 where
  total       = length . filter (/= '.') $ A.elems res
  dragonSpots = length . filter (== 'x') $ A.elems res
  res         = AS.runSTUArray $ do
    state <- AS.newListArray ((0, 0), bounds) canvas

    forM_
      offset
      (\(x0, y0) -> do
        let dragon2 = map (\(x, y) -> (x + x0, y + y0)) dragon
        found <- notElem '.' <$> forM dragon2 (AS.readArray state)
        when found $ forM_ dragon2 (\pos -> AS.writeArray state pos 'x')
      )
    pure state

fromBinary :: [Int] -> Int
fromBinary [] = 0
fromBinary x  = foldl1 (\acc e -> acc * 2 + e) x

