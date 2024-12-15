module Day.Day14 (run) where

import Control.Arrow ((>>>))
import Control.Lens hiding (noneOf)
import Control.Monad (void)
import Control.Monad.Extra
import Data.Containers.ListUtils
import Data.Foldable
import Data.List
import Data.List.Extra (groupOn)
import Data.List.Split (splitOneOf)
import Data.Map qualified as Map
import Data.Maybe
import Data.Mod.Word
import Data.Ord
import Linear
import Map (fromListOnWith)
import Print
import Safe
import Test.HUnit ((@=?))
import Text.Megaparsec (
  MonadParsec (try),
  many,
  noneOf,
  sepEndBy,
  (<|>),
 )
import Text.RawString.QQ (r)
import Utils

data Robot = Robot (V2 Int) (V2 Int) deriving (Show, Eq, Ord)

parse = lines >>> fmap (splitOneOf "=, v" >>> mapMaybe (readMay @Int) >>> toRobot)
 where
  toRobot [px, py, vx, vy] = Robot (V2 px py) (V2 vx vy)

-- lims = V2 101 103

limX = if test then 11 else 101
limY = if test then 7 else 103

xQuadLimit = (limX `div` 2)
yQuadLimit = (limY `div` 2)

moveRobot (Robot p v) = Robot (v2Mod (p + v)) v

v2Mod (V2 x y) = V2 (mod x limX) (mod y limY)

f x | x == 1 = 1

toQuad (Robot (V2 x y) _) | x == xQuadLimit || y == yQuadLimit = Nothing
toQuad (Robot (V2 x y) _) = Just (x < xQuadLimit, y < yQuadLimit)

defMap = Map.fromList $ do
  x <- [0 .. limX - 1]
  y <- [0 .. limY - 1]
  pure (V2 x y, ' ')

solveA robots = do
  let moved = take (if test then 101 else 10404) $ iterate (fmap moveRobot) robots
  let count c = fromListOnWith (const (1 :: Int)) (+) $ mapMaybe toQuad c
  -- let after100 = moved !! 100
  -- print $ product (count after100)

  let countNothings c = length $ filter (== Nothing) $ map toQuad c

  let quadOk = const True -- (== 2) . length . nubOrd . Map.elems . count
       where

  let getXCount = map (\(Robot p _) -> view _x p) >>> sort >>> group >>> fmap length >>> maximum

  -- sum xBigs == sum xSmalls && sum yBigs == sum ySmalls

  -- co = count c
  -- (xBigs, xSmalls) = Map.partitionWithKey (\k _ -> fst k) co
  -- (yBigs, ySmalls) = Map.partitionWithKey (\k _ -> snd k) co

  let sorted = sortOn (Down . getXCount . snd) (zip [0 ..] moved)

  -- print "OK"
  -- print $ length $ filter quadOk moved
  print "----"

  -- print $ findIndex (== robots) $ tail moved

  ifor sorted $ \_ (i, cur) -> do
    when (quadOk cur) $ do
      let newMap = foldMap (\(Robot p _) -> Map.singleton p 'x') cur <> defMap
      print i
      printV2M newMap

printV2M m = do
  putStrLn "--------"
  let xs = Map.toList m
  let g = groupOn (\(V2 _ y, _) -> y) $ sortOn (\(V2 x y, _) -> V2 y x) xs
  let gg = fmap (fmap snd) g
  mapM_
    ( \x -> do
        putStr x
        putStrLn ""
    )
    $ gg
  putStrLn ""

solveB = id

testInput =
  [r|p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
|]

-- testInput2 = "p=2,4 v=2,-3"

test = False

run :: String -> IO ()
run input = void $ do
  -- input <- putStrLn "#####    testInput   #####" >> pure testInput
  -- print input
  let parsed = parse input
  -- mprint parsed
  let resA = solveA parsed
  resA

  print $ xQuadLimit
  print $ yQuadLimit

-- print $ V2 (3 :: Int) 2 `mod` V2 2 1

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
