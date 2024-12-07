module Day.Day07 (run) where

import Control.Arrow ((>>>))
import Control.Lens ((&))
import Control.Monad (void)
import Control.Monad.Extra (void)
import Data.List.Extra (partition, splitOn, sumOn')
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)

data Equation = Int :== [Int]

parse :: String -> [Equation]
parse = lines >>> map parseEq
 where
  parseEq (splitOn ": " -> [left, rightNums]) = read left :== (rightNums & words & map read)

hasSolution :: Bool -> Equation -> Bool
hasSolution canConcat (target :== (n : nums)) = go n nums
 where
  go cur [] = cur == target
  go cur (x : xs) | cur > target || x > target = False
  go cur (x : xs) = (canConcat && go (concatNumbers cur x) xs) || go (cur * x) xs || go (cur + x) xs

concatNumbers :: Int -> Int -> Int
concatNumbers a b = a * 10 ^ (floor (logBase 10 (fromIntegral b)) + 1) + b

run :: String -> IO ()
run input = void $ do
  let parsed = parse input
  let (oks, nos) = parsed & partition (hasSolution False)
  let resA = oks & sumOn' (\case l :== _ -> l)
  print $ resA
  resA @=? 1430271835320

  let resB = resA + (nos & filter (hasSolution True) & sumOn' (\case l :== _ -> l))
  print resB
  resB @=? 456565678667482