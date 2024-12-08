module Day.Day08 (run) where

import Control.Lens ((&))
import Control.Monad (guard, void)
import Data.List (nub, sortOn)
import Data.List.Extra (groupOn, nubOrd)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Linear (V2)
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils (countP, parseAsciiMap)

countAntinodes :: ((V2 Int -> V2 Int) -> V2 Int -> [V2 Int]) -> Map.Map (V2 Int) Char -> _
countAntinodes iterator grid = length $ nubOrd $ do
  ((p1, l1), (p2, l2)) <- grid & Map.filter (/= '.') & Map.toList & diagonalPairs

  guard (l1 == l2)

  makeAntinodes p1 p2
 where
  makeAntinodes c1 c2 = createAllValidAntinodes (c1 - c2) c1 <> createAllValidAntinodes (c2 - c1) c2

  createAllValidAntinodes v c = takeWhile (`Map.member` grid) $ (iterator (+ v) c)

diagonalPairs :: [a] -> [(a, a)]
diagonalPairs [] = []
diagonalPairs (x : xs) = fmap (x,) xs ++ diagonalPairs xs

run :: String -> IO ()
run input = void $ do
  let grid = parseAsciiMap Just input

  let resA = countAntinodes (\f x -> [f x]) grid
  print resA
  resA @=? 364

  let resB = countAntinodes iterate grid
  print resB
  resB @=? 1231
