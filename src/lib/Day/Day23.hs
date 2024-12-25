module Day.Day23 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (guard, void)
import Data.List (intercalate, isPrefixOf, partition)
import Data.List.Extra (maximumOn, splitOn)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Test.HUnit ((@=?))
import Utils (apply2)

parse :: String -> Map.Map [Char] (Set [Char])
parse =
  lines
    >>> concatMap (splitOn "-" >>> apply2 (\x t -> [(x, t), (t, x)]))
    >>> map (over _2 Set.singleton)
    >>> Map.fromListWith (<>)

getPartitions graph [] = []
getPartitions graph (x : xs) =
  let xset = graph Map.! x
      (ts, fs) = partition (\y -> Set.member y xset && Set.member x (graph Map.! y)) xs
   in (x : ts) : getPartitions graph fs

solveA graph = do
  let get3s = Set.fromList $ do
        (k, vs) <- Map.toList $ Map.map Set.toList graph
        (a, b) <- twos vs
        guard $ Set.member a (graph Map.! b) && Set.member b (graph Map.! a)
        pure $ Set.fromList [k, a, b]

  let getres = Set.size . Set.filter (any (isPrefixOf "t"))

  getBigs graph & foldMap Set.powerSet & Set.filter (\x -> Set.size x == 3) & getres

getBigs graph = Set.fromList $ do
  (k, vs) <- Map.toList $ Map.map Set.toList graph
  parts <- getPartitions graph vs
  pure $ Set.fromList (k : parts)

solveB graph = do
  intercalate "," $ Set.toList $ maximumOn Set.size $ Set.toList $ getBigs graph

twos [] = []
twos (x : xs) = map (x,) xs ++ twos xs

run :: String -> IO ()
run input = void $ do
  let parsed = parse input

  let resA = solveA parsed
  print resA
  resA @=? 1248

  let resB = solveB parsed
  print resB
  resB @=? "aa,cf,cj,cv,dr,gj,iu,jh,oy,qr,xr,xy,zb"
