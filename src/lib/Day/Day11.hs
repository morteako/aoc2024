module Day.Day11 (run) where

import Control.Arrow ((>>>))
import Control.Lens (ifoldMapByOf, ifolded, (<&>))
import Control.Monad (void)
import Data.Foldable (Foldable (fold))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Test.HUnit ((@=?))

parse :: String -> [Int]
parse = words >>> map read

(<+>) :: IntMap Int -> IntMap Int -> IntMap Int
(<+>) = IntMap.unionWith (+)

getStoneCounts :: [Int] -> _
getStoneCounts xs = iterate calculateStones stoneMap <&> sum
 where
  calculateStones = ifoldMapByOf ifolded (<+>) mempty blink
  stoneMap = foldMap (`IntMap.singleton` 1) xs

blink :: Int -> Int -> IntMap Int
blink 0 count = IntMap.singleton 1 count
blink stoneNumber count
  | let d = digits stoneNumber
  , even d
  , let (a, b) = splitNumber d stoneNumber =
      IntMap.singleton a count <+> IntMap.singleton b count
blink stoneNumber count = IntMap.singleton (stoneNumber * 2024) count

digits :: Int -> Int
digits 0 = 1
digits n = length $ takeWhile (> 0) $ iterate (`div` 10) n

splitNumber :: Int -> Int -> (Int, Int)
splitNumber digitNumber n = (n `div` divisor, n `mod` divisor)
 where
  halfCount = digitNumber `div` 2
  divisor = 10 ^ (digitNumber - halfCount)

run :: String -> IO ()
run input = void $ do
  let stones = parse input

  let stoneCounts = getStoneCounts stones

  let resA = stoneCounts !! 25
  print resA

  resA @=? 217812

  let resB = stoneCounts !! 75
  print resB
  resB @=? 259112729857522