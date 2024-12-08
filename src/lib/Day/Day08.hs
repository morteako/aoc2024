module Day.Day08 (run) where

import Control.Lens ((&))
import Control.Monad (void)
import Data.List (sortOn)
import Data.List.Extra (groupOn)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Linear (V2)
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils (countP, parseAsciiMap)

countAntinodes :: ((V2 Int -> V2 Int) -> V2 Int -> [V2 Int]) -> Map.Map (V2 Int) Char -> Int
countAntinodes iterator grid = allPosPairs & foldMap makeAntinodes & countP (`Map.member` grid)
 where
  allPosPairs =
    grid
      & Map.filter (/= '.')
      & Map.toList
      & sortOn snd
      & groupOn snd
      & fmap (fmap fst)
      & concatMap allPairs

  makeAntinodes (c1, c2) = createAllValidAntinodes (-v) c1 <> createAllValidAntinodes v c2
   where
    v = c2 - c1

  createAllValidAntinodes v c = Set.fromList $ takeWhile (`Map.member` grid) $ (iterator (+ v) c)

allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x : xs) = fmap (x,) xs ++ allPairs xs

run :: String -> IO ()
run input = void $ do
  let grid = parseAsciiMap Just input

  let resA = countAntinodes (\f x -> [f x]) grid
  print resA
  resA @=? 364

  let resB = countAntinodes iterate grid
  print resB
  resB @=? 1231
