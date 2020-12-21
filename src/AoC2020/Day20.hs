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

data Tile = Tile {ops :: String, raw :: [String], index :: Int ,up :: Int, right :: Int, down :: Int, left :: Int} deriving (Eq, Ord)

instance Show Tile where
  show (Tile o _ i a b c d) =
    "Tile (index:"
      ++ show i
      ++ " ops:"
      ++ o
      ++ " "
      ++ show a
      ++ " "
      ++ show b
      ++ " "
      ++ show c
      ++ " "
      ++ show d
      ++ ")"

transformRaw ops = transformRaw' (reverse ops)

transformRaw' :: String -> [String] -> [String]
transformRaw' []           = id
transformRaw' ('r' : rest) = transformRaw' rest . map reverse . transpose
transformRaw' ('f' : rest) = transformRaw' rest . map reverse

tileToBinary (Tile _ _ _ a b c d) =
  [toBinary2 10 a, toBinary2 10 b, toBinary2 10 c, toBinary2 10 d]

showTile tile =
  let [top, right, bot, left] = tileToBinary tile
  in  do
        putStr " " >> putStrLn top
        let x = zipWith (\a b -> pure a ++ replicate 10 ' ' ++ pure b)
                        left
                        right
        mapM_ putStrLn x
        putStr " " >> putStrLn bot

rotateTile (Tile ops raw i a b c d) =
  Tile ('r' : ops) raw i (reverseBits d) a (reverseBits b) c
flipTile (Tile ops raw i a b c d) =
  Tile ('f' : ops) raw i (reverseBits a) d (reverseBits c) b

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
valid state (x, y) = filter (validTile (x, y) state) . getOrientations

validTile (x, y) state t = and $ mapMaybe
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
  inputData <- fst . head . readP_to_S (parser <* eof) <$> readFile
    "src/AoC2020/Day20.txt"

  let squares  = map squareToTile inputData

  let testTile = head squares

  let order    = 12 

  let Just run = solve M.empty (makeOrder order) $ map squareToTile inputData

  let ans =
        product $ map (index . (run M.!)) [(0, 0), (11, 0), (0, 11), (11, 11)]

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

  let canvasArray :: A.Array (Int, Int) Char
      canvasArray =
        A.listArray ((0, 0), (8 * order - 1, 8 * order - 1)) . concat $ canvas

  let
    monster =
      map fst
        . filter ((== '#') . snd)
        . concat
        . zipWith (\i -> zipWith (\j -> ((i, j), )) [0 ..]) [0 ..]
--        $["##","##"]
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
  let bbbb = head . sort $  map aaaa trans

  putStr "part two: "
  print bbbb

--checkMonster :: (Int, Int) -> String -> [(Int, Int)] -> [(Int, Int)] -> Int
checkMonster bounds canvas dragon offset = total - dragonSpots
 where
  total       = length . filter (/= '.') $ A.elems res
  dragonSpots = length . filter (== 'x') $ A.elems res
  res         = AS.runSTUArray $ do
    state <-
      AS.newListArray ((0, 0), bounds) canvas :: ST
        s
        (AS.STUArray s (Int, Int) Char)

    forM_
      offset
      (\(x0, y0) -> do
        let dragon2 = map (\(x, y) -> (x + x0, y + y0)) dragon
        aaaaaaa <- notElem '.' <$> forM dragon2 (AS.readArray state)
        when aaaaaaa $ forM_ dragon2 (\pos -> AS.writeArray state pos 'x')
      )

    pure state



fromBinary :: [Int] -> Int
fromBinary [] = 0
fromBinary x  = foldl1 (\acc e -> acc * 2 + e) x

toBinary :: Int -> Int -> [Int]
toBinary pad 0 = replicate pad 0
toBinary pad n | n `mod` 2 == 0 = toBinary (pad - 1) (n `div` 2) ++ [0]
               | otherwise      = toBinary (pad - 1) (n `div` 2) ++ [1]

toBinary2 :: Int -> Int -> String
toBinary2 pad 0 = replicate pad '.'
toBinary2 pad n | n `mod` 2 == 0 = toBinary2 (pad - 1) (n `div` 2) ++ "."
                | otherwise      = toBinary2 (pad - 1) (n `div` 2) ++ "#"

applyEnds :: (a -> a) -> ([a] -> [a]) -> [a] -> [a]
applyEnds f1 f2 l = begin : mid ++ [end] where
  begin = f1 $ head l
  mid   = f2 . init $ drop 1 l
  end   = f1 $ last l

mark :: [String] -> [String]
mark s = s2 where s2 = applyEnds id (map (applyEnds id (' ' <$))) s

inscribe :: String -> [String] -> [String]
inscribe i (s1 : s2 : (s : s3) : rest) = s1 : s2 : (s : weirdZip) : rest
 where
  digits   = Just ' ' : map Just i ++ repeat Nothing
  weirdZip = zipWith
    (\maybeChar c -> case maybeChar of
      Just c1 -> c1
      _       -> c
    )
    digits
    s3


