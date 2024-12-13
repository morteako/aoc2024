module Day.Day12 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (guard, void)
import Data.List (partition, sort)
import Data.List.Extra (groupOn, partition, sort, sumOn')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Linear (V2 (..))
import Test.HUnit ((@=?))
import Utils (parseAsciiMap)

parse :: String -> Map (V2 Int) Char
parse = parseAsciiMap Just

neighs :: (Num a) => V2 a -> [V2 a]
neighs v = map (+ v) [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

splitIntoGroups :: (Ord a) => (Set a -> Set a) -> Set a -> [Set a]
splitIntoGroups next mainSet =
  case Set.minView mainSet of
    Nothing -> []
    Just (minP, _) ->
      let
        group = expandGroup mainSet (Set.singleton minP)
       in
        group : splitIntoGroups next (mainSet `Set.difference` group)
 where
  expandGroup mainSet curs = do
    let allNews = curs <> next curs
    let valids = allNews `Set.intersection` mainSet
    if Set.size valids > Set.size curs
      then
        expandGroup mainSet valids
      else
        curs

type GroupId = (Char, Int)

getGroupedSidesAndAllPositions :: Map (V2 Int) Char -> [([[V2 Int]], Set (V2 Int))]
getGroupedSidesAndAllPositions grid = do
  let
    groups :: Map Char [Set (V2 Int)]
    groups =
      grid
        & Map.foldrWithKey (\pos v -> Map.insertWith (<>) v (Set.singleton pos)) mempty
        & Map.map (splitIntoGroups (foldMap (Set.fromList . neighs)))

  let lettersToPos :: Map GroupId (Set (V2 Int))
      lettersToPos = groups & Map.foldMapWithKey (\k poses -> imap (\i p -> ((k, i), p)) poses) & Map.fromList

  let posToLetter :: Map (V2 Int) GroupId
      posToLetter =
        groups
          & Map.foldMapWithKey (\k poses -> imap (\i p -> (p, (k, i))) poses)
          & foldMap (\(poses, letter) -> foldMap (`Map.singleton` letter) poses)

  lettersToPos
    & Map.foldMapWithKey (\k poses -> [(getGroupedSides posToLetter lettersToPos k, poses)])

getGroupedSides :: Map (V2 Int) GroupId -> Map GroupId (Set (V2 Int)) -> GroupId -> [[V2 Int]]
getGroupedSides posToLetter letterToPos indexedLetter = concatMap splitGroupWhenDisjoin allGroups
 where
  allGroups = map (map snd) $ groupOn fst $ sort $ do
    p <- Set.toList (letterToPos Map.! indexedLetter)
    dir <- [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]
    let next = p + dir
    let nextLetter = posToLetter Map.!? next
    guard $ nextLetter /= Just indexedLetter
    pure (dir, next)

splitGroupWhenDisjoin :: [V2 Int] -> _
splitGroupWhenDisjoin (g : group) = f [g] group
 where
  f curs@(cur : _) g = case partition ((isNeigh cur)) g of
    ([], []) -> [curs]
    (x : xs, rest) -> f (x : curs) (xs ++ rest)
    ([], r : rest) -> curs : f [r] rest

  isNeigh a b = 1 == (sum $ abs (a - b))

solveA :: [([[a1]], Set a2)] -> Int
solveA =
  sumOn' (\(groupedSides, allPositions) -> sumOn' length groupedSides * length allPositions)

solveB :: [([a1], Set a2)] -> Int
solveB = do
  sumOn' (\(groupedSides, allPositions) -> length groupedSides * length allPositions)

run :: String -> IO ()
run input = void $ do
  let parsed = parse input

  let grouped = getGroupedSidesAndAllPositions parsed
  let resA = solveA grouped
  print resA
  resA @=? 1375476

  let resB = solveB grouped
  print resB
  resB @=? 821372
