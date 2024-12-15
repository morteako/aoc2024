module Day.Day14 (run) where

import Control.Arrow ((>>>))
import Control.Lens hiding (noneOf)
import Control.Monad (void)
import Control.Monad.Extra
import Data.List (group, sort, sortOn)
import Data.List.Extra (groupOn)
import Data.List.Split (splitOneOf)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Mod.Word
import Data.Ord
import Linear
import Map (fromListOnWith)
import Print
import Safe
import Test.HUnit ((@=?))
import Utils

data Robot = Robot (V2 Int) (V2 Int) deriving (Show, Eq, Ord)

parse :: String -> [Robot]
parse = lines >>> fmap (splitOneOf "=, v" >>> mapMaybe (readMay @Int) >>> toRobot)
 where
  toRobot [px, py, vx, vy] = Robot (V2 px py) (V2 vx vy)

limX, limY :: Int
limX = 101
limY = 103

xQuadLimit, yQuadLimit :: Int
xQuadLimit = (limX `div` 2)
yQuadLimit = (limY `div` 2)

moveRobot :: Robot -> Robot
moveRobot (Robot p v) = Robot (v2Mod (p + v)) v

v2Mod :: V2 Int -> V2 Int
v2Mod (V2 x y) = V2 (mod x limX) (mod y limY)

toQuadrant :: Robot -> Maybe (Bool, Bool)
toQuadrant (Robot (V2 x y) _) | x == xQuadLimit || y == yQuadLimit = Nothing
toQuadrant (Robot (V2 x y) _) = Just (x < xQuadLimit, y < yQuadLimit)

solveA :: [Robot] -> Int
solveA robots = do
  let moved = (!! 100) $ iterate (fmap moveRobot) robots
  let countQuadrants c = fromListOnWith (const 1) (+) $ mapMaybe toQuadrant c

  moved & countQuadrants & product

findChristmasTreeMess :: [Robot] -> IO [()]
findChristmasTreeMess robots = do
  let moved = iterate (fmap moveRobot) robots
  let countQuadrants c = fromListOnWith (const 1) (+) $ mapMaybe toQuadrant c

  let countNothings c = length $ filter (== Nothing) $ map toQuadrant c

  let getXCount = map (\(Robot p _) -> view _x p) >>> sort >>> group >>> fmap length >>> maximum

  let limit = 1000

  let sorted = sortOn (Down . getXCount . snd) (take limit $ zip [0 ..] moved)

  let defMap = Map.fromList $ do
        x <- [0 .. limX - 1]
        y <- [0 .. limY - 1]
        pure (V2 x y, ' ')

  ifor sorted $ \_ (i, cur) -> do
    let newMap = foldMap (\(Robot p _) -> Map.singleton p 'x') cur <> defMap
    print i
    printV2M newMap

printV2M :: (Ord a) => Map.Map (V2 a) Char -> IO ()
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

run :: String -> IO ()
run input = void $ do
  let parsed = parse input
  let resA = solveA parsed
  print resA

  resA @=? 219150360

  -- part B
  findChristmasTreeMess parsed
