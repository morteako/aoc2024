module Day.Day18 (run) where

import Control.Arrow ((>>>))
import Control.Monad (guard, void)
import Data.Foldable (Foldable (toList))
import Data.Graph.AStar (aStar)
import Data.HashSet qualified as HashSet
import Data.List (findIndex, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Linear (V2 (..))
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils (readInt)

parse :: String -> [V2 Int]
parse = dots
 where
  dots = lines >>> map (splitOn "," >>> \[a, b] -> readInt <$> V2 a b)

solveA :: Int -> Int -> [V2 Int] -> Int
solveA limit numBytes bytes = do
  let byteGrid = Set.fromList $ take numBytes bytes
  let start = 0
  let end = V2 limit limit

  let isInside x = x >= 0 && x <= limit

  let isGoal = (== end)
  let neighDist a b = 1
  let heur t = sum $ abs $ end - t
  let neighs (V2 x y) = HashSet.fromList $ do
        v@(V2 xx yy) <- [V2 (succ x) y, V2 (pred x) y, V2 x (succ y), V2 x (pred y)] :: [V2 Int]
        guard $ isInside xx && isInside yy && Set.notMember v byteGrid
        pure v

  let Just res = aStar neighs neighDist heur isGoal start
  length res

solveB :: Int -> Int -> [V2 Int] -> String
solveB limit numBytes bytes = do
  let byteGrids = drop numBytes $ tail $ scanl (flip Set.insert) mempty bytes
  let start = 0
  let end = V2 limit limit

  let isInside x = x >= 0 && x <= limit

  let isGoal = (== end)
  let neighDist a b = 1
  let heur t = sum $ abs $ end - t
  let isBlocked byteGrid = isNothing $ aStar neighs neighDist heur isGoal start
       where
        neighs (V2 x y) = HashSet.fromList $ do
          v@(V2 xx yy) <- [V2 (succ x) y, V2 (pred x) y, V2 x (succ y), V2 x (pred y)] :: [V2 Int]
          guard $ isInside xx && isInside yy && Set.notMember v byteGrid
          pure v

  let Just ((+ numBytes) -> blockIndex) = findIndex isBlocked byteGrids

  intercalate "," $ fmap show $ toList $ bytes !! blockIndex

run :: String -> IO ()
run input = void $ do
  let limit = 70
  let numBytes = 1024
  let parsed = parse input

  let resA = solveA limit numBytes parsed
  print resA
  resA @=? 356

  let resB = solveB limit numBytes parsed
  putStrLn resB
  resB @=? "22,33"
