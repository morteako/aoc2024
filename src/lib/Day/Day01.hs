module Day.Day01 (run) where

import Control.Arrow ((>>>))
import Control.Lens (Each (each), over)
import Control.Monad (void)
import Data.List.Extra (sort, splitOn, sumOn')
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils (count)

parse :: String -> ([Int], [Int])
parse = lines >>> map (words >>> map read >>> f) >>> unzip
 where
  f [x, y] = (x, y)

solveA :: ([Int], [Int]) -> Int
solveA = over each sort >>> uncurry (zipWith (\a b -> abs (a - b))) >>> sum

solveB :: ([Int], [Int]) -> Int
solveB (lefts, rights) = sumOn' (\x -> x * count x rights) lefts

run :: String -> IO ()
run input = void $ do
  let parsed = parse input
  let resA = solveA parsed
  print resA
  resA @=? 2086478
  let resB = solveB parsed
  print resB
  resB @=? 24941624
