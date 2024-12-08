module Day.Day08 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (void)
import Data.List
import Data.List.Extra (groupOn)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid
import Data.Set qualified as Set
import Linear
import Print
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils

-- data Tile = Dot | Letter Char deriving Show

parse = parseAsciiMap Just

solveA grid = antinodes & Set.fromList & countP (`Map.member` grid)
 where
  antinodes = (allPosPairs & foldMap makeAntinodes)

  allPosPairs =
    grid
      & Map.filter (/= '.')
      & Map.toList
      & sortOn snd
      & groupOn snd
      & fmap (fmap fst)
      & concatMap allPairs

  makeAntinodes (c1, c2) = [c1 - v] ++ [c2 + v]
   where
    v = c2 - c1

solveB grid = antinodes & Set.fromList & Set.size
 where
  antinodes = (allPosPairs & foldMap makeAntinodes)

  allPosPairs =
    grid
      & Map.filter (/= '.')
      & Map.toList
      & sortOn snd
      & groupOn snd
      & fmap (fmap fst)
      & concatMap allPairs

  makeAntinodes (c1, c2) = f (-v) c1 ++ f v c2
   where
    f vv c = takeWhile (`Map.member` grid) $ (iterate (+ vv) c1)
    v = c2 - c1

allPairs [] = []
allPairs (x : xs) = fmap (x,) xs ++ allPairs xs

testInput =
  [r|............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
|]

run :: String -> IO ()
run input = void $ do
  let parsed = parse input

  let resA = solveA parsed
  print resA
  resA @=? 364

  let resB = solveB parsed
  print resB
  resB @=? 1231
