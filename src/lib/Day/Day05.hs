module Day.Day05 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (void, when)
import Control.Monad.State
import Data.Foldable.Extra (sumOn', traverse_)
import Data.List (scanl', sortBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils (readInt, toTuple)

parse :: [Char] -> (Map Int (Set Int), [[Int]])
parse input =
  (parseRules ruleLines, parseOrders orderLines)
 where
  [ruleLines, orderLines] =
    input & splitOn "\n\n" & map lines

  parseRules =
    map parseOrderRule
      >>> map (over _2 Set.singleton)
      >>> (Map.fromListWith (<>) >>> id)

  parseOrders = map (splitOn "," >>> map readInt)
  parseOrderRule = splitOn "|" >>> map readInt >>> toTuple

solveA :: Map Int (Set Int) -> [[Int]] -> Int
solveA rules orders =
  filter (isInOrder rules) orders
    & sumOn' getMiddle

isInOrder :: Map Int (Set Int) -> [Int] -> Bool
isInOrder rules order = all isInOrder' elementsWithRestSets
 where
  elementsWithRestSets = zip order $ tail $ scanr Set.insert Set.empty order

  isInOrder' (cur, restSet) = Set.isSubsetOf restSet (Map.findWithDefault Set.empty cur rules)

getMiddle :: [a] -> a
getMiddle xs = xs !! div (length xs) 2

solveB :: Map Int (Set Int) -> [[Int]] -> Int
solveB rules orders =
  filter (not . isInOrder rules) orders
    & map (sortBy compareOrder)
    & sumOn' getMiddle
 where
  compareOrder a b
    | Set.member b (Map.findWithDefault mempty a rules) = LT
    | otherwise = GT

run :: String -> IO ()
run input = void $ do
  let (rules, orders) = parse input

  let resA = solveA rules orders
  print resA
  resA @=? 5248

  let resB = solveB rules orders
  print resB
  resB @=? 4507
