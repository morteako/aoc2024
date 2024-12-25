module Day.Day22 (run) where

import Control.Arrow ((>>>))
import Control.Lens ((&))
import Control.Monad (void)
import Data.Bits (Bits (xor))
import Data.List.Split (divvy)
import Data.Map qualified as Map
import Test.HUnit ((@=?))
import Utils (readInt)

parse :: String -> [Int]
parse = lines >>> map readInt

secretNumber :: Int -> Int
secretNumber = mixPrune (* 64) >>> mixPrune (flip div 32) >>> mixPrune (* 2048)
 where
  mixPrune f x = (f x `xor` x) `mod` 16777216

solveA :: [Int] -> Int
solveA = map (iterate secretNumber >>> take numIterations >>> last) >>> sum

numIterations :: Int
numIterations = 2001

solveB :: [Int] -> _
solveB nums = do
  let maps = do
        num <- nums
        let secrets = iterate secretNumber num & map (flip mod 10)
        let seqs = secrets & diffs & divvy 4 1
        pure $ Map.fromListWith (\_new old -> old) $ take numIterations $ zip seqs (drop 4 secrets)
  maximum $ Map.unionsWith (+) maps

diffs :: [Int] -> [Int]
diffs xs = zipWith subtract xs (tail xs)

run :: String -> IO ()
run input = void $ do
  let parsed = parse input

  let resA = solveA parsed
  print resA
  resA @=? 17612566393

  let resB = solveB parsed
  print resB
  resB @=? 1968
