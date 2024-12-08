module Day.Day07Backwards (run) where

import Control.Arrow ((>>>))
import Control.Lens ((&))
import Control.Monad (void)
import Control.Monad.Extra (void)
import Data.Digits (digits, unDigits)
import Data.List.Extra (isPrefixOf, isSuffixOf, partition, splitOn, stripSuffix, sumOn')
import Data.Maybe (maybeToList)
import Test.HUnit ((@=?))

data Equation = Int :== [Int]
parse :: String -> [Equation]
parse = lines >>> map parseEq
 where
  parseEq (splitOn ": " -> [left, rightNums]) = read left :== (rightNums & words & map read)

hasSolution :: Bool -> Equation -> Bool
hasSolution canConcat (target :== nums) = go [target] $ reverse nums
 where
  go :: [Int] -> [Int] -> Bool
  go [] _ = False
  go targets [x] = elem x targets
  go targets (x : xs) =
    let
      divved = [c `div` x | c <- targets, c `mod` x == 0]
      added = [c - x | c <- targets, let res = c - x, res >= 0]
      concatted =
        [ unDigits 10 suff
        | canConcat
        , c <- targets
        , suff <- maybeToList (digits 10 x `stripSuffix` digits 10 c)
        ]
      allCurs = (added ++ divved ++ concatted)
     in
      go allCurs xs

run :: String -> IO ()
run input = void $ do
  let parsed = parse input
  let (oks, nos) = parsed & partition (hasSolution False)
  let resA = oks & sumOn' (\case l :== _ -> l)
  print $ resA
  resA @=? 1430271835320

  let resB = (parsed & filter (hasSolution True) & sumOn' (\case l :== _ -> l))
  print resB

  resB @=? 456565678667482
