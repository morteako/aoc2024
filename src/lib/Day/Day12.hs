module Day.Day12 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (guard, void)
import Control.Monad.State
import Data.Foldable (for_, traverse_)
import Data.Graph.Inductive (neighbors)
import Data.List
import Data.List.Extra
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid (Sum (getSum))
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple
import Debug.Trace
import Linear
import Map qualified
import Print
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils

parse :: String -> Map (V2 Int) Char
parse = parseAsciiMap Just

neighs v = map (+ v) [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

neighsSet xy = Set.fromList $ neighs xy

neighs2 v = map (\x -> (x + v, v)) [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

neighsMap xs = Map.fromListWith (+) . map (,1) . neighs $ xs

(<<>>) = Map.unionWith (<>)

getAllGroups :: (Num a, Ord a) => Set (V2 a) -> [Set (V2 a)]
getAllGroups letterGroup =
  case Set.minView letterGroup of
    Nothing -> []
    Just (minP, _) ->
      let
        r = getGroup letterGroup (Set.singleton minP)
       in
        r : getAllGroups (letterGroup `Set.difference` r)
 where
  getGroup allLetters curs = do
    let allNews = curs <> foldMap (neighsSet) curs
    let valids = allNews `Set.intersection` allLetters
    if Set.size valids > Set.size curs
      then
        getGroup allLetters valids
      else
        curs

-- getOutside org = Map.withoutKeys news org
--  where
--   news = Map.unionsWith (+) $ map neighsMap $ Set.toList org

type Point = V2 Int

solveA :: Map (V2 Int) Char -> Int
solveA grid = do
  let fgrid = grid & Map.foldrWithKey (\pos v d -> (Map.singleton v (Set.singleton pos)) <<>> d) mempty

  let grouped = Map.map getAllGroups fgrid

  let getInfo2 s = (actualArea * length outside)
       where
        actualArea = Set.size s
        outside = getOutsideFlattened (getOutside2 s)

  grouped & concat & map getInfo2 & sum

-- neighsDirMap p = Map.fromListWith (+) . map (\x -> (x + p, x)) $ [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

getOutside2 :: Set (V2 Int) -> Map (V2 Int) [V2 Int]
getOutside2 org = Map.withoutKeys news org
 where
  news = Map.unionsWith (<>) $ map neighsMap $ Set.toList org
  neighsMap pos = Map.fromListWith (<>) . map (\x -> (x + pos, ([x]))) $ [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

getOutsideFlattened :: Map (V2 Int) [V2 Int] -> Set (V2 Int, V2 Int)
getOutsideFlattened grid = Map.toList grid & concatMap (\(x, xs) -> map (x,) xs) & Set.fromList

solveB :: Map (V2 Int) Char -> IO ()
solveB grid = do
  let fgrid = grid & Map.foldrWithKey (\pos v d -> (Map.singleton v (Set.singleton pos)) <<>> d) mempty

  let grouped = Map.map getAllGroups fgrid

  let lettersToPos = grouped & Map.foldMapWithKey (\k poses -> imap (\i p -> ((k, i), p)) poses) & Map.fromListWith undefined

  let posToLetter :: Map (V2 Int) (Char, Int)
      posToLetter =
        grouped
          & Map.foldMapWithKey (\k poses -> imap (\i p -> (p, (k, i))) poses)
          & concatMap (\(poses, letter) -> foldMap (\p -> [(p, letter)]) poses)
          & Map.fromListWith undefined

  mprint $ lettersToPos
  mprint $ posToLetter

  print "sidegroups"
  -- -- mprint $ getSideGroups posToLetter lettersToPos ('X', 0)
  -- mprint $ getSideGroups posToLetter lettersToPos ('E', 0) & map (\x -> (x, length x))
  -- mprint $ getSideGroups posToLetter lettersToPos ('E', 0) & length
  -- mprint $ getSideGroups posToLetter lettersToPos ('X', 0) & length
  -- mprint $ getSideGroups posToLetter lettersToPos ('X', 1) & length

  let qqq = "RICFVJCEIMS"

  let res =
        lettersToPos
          & Map.mapWithKey (\k poses -> swap (length (getSideGroups posToLetter lettersToPos k), length poses))
  -- & Map.elems

  mprint $
    res
      -- & Map.toList
      -- & sortOn (\(x, _) -> elemIndex (fst x) qqq) --
      & Map.elems
      & sumOn' (uncurry (*))

-- (('I',0),(14,17)) skal være 14 16
-- I 1 4 5 skal være 4 4
-- F 10 13 - skal være 10 12
-- V 13 12 skal være 13 10
-- J 11 15 skal være 11 12
--

type IndexLetter = (Char, Int)

type PosIndexMap = Map (Set (V2 Int)) (Char, Int)

getSideGroups :: Map (V2 Int) (Char, Int) -> Map IndexLetter (Set (V2 Int)) -> IndexLetter -> _
getSideGroups posToLetter letterToPos indexedLetter = concatMap splitGroup allHits
 where
  allHits = map (map snd) $ groupOn fst $ sort $ do
    p <- Set.toList letterPoses
    dir <- [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]
    let next = p + dir
    let nextLetter = posToLetter Map.!? next
    guard $ nextLetter /= Just indexedLetter
    pure ((dir), next)

  letterPoses = letterToPos Map.! indexedLetter

-- OLD
-- getSideGroups :: Map (V2 Int) (Char, Int) -> Map IndexLetter (Set (V2 Int)) -> IndexLetter -> _
-- getSideGroups posToLetter letterToPos indexedLetter = concatMap splitGroup allHits
--  where
--   allHits = map (map snd) $ groupOn fst $ sort $ do
--     p <- Set.toList letterPoses
--     dir <- [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]
--     let next = p + dir
--     let nextLetter = posToLetter Map.!? next
--     guard $ nextLetter /= Just indexedLetter
--     pure ((nextLetter, dir), next)

--   letterPoses = letterToPos Map.! indexedLetter

-- splitGroup2 = groupBy

splitGroup :: [V2 Int] -> _
splitGroup (g : group) = f [g] group
 where
  f curs@(cur : _) g = case partition (\p -> any (isNeigh p) curs) g of
    ([], []) -> [curs]
    (x : xs, rest) -> f (x : curs) (xs ++ rest)
    ([], r : rest) -> curs : f [r] rest

isNeigh a b = 1 == (sum $ abs (a - b))

testInput =
  [r|AAAA
BBCD
BBCC
EEEC|]

testInput2 =
  [r|OOOOO
OXOXO
OOOOO
OXOXO
OOOOO|]

testInput3 =
  [r|RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE|]

-- 4+4=8
testInput4 =
  [r|
AAA
AXA
AAA|]

testInput5 =
  [r|EEEEE
EXXXX
EEEEE
EXXXX
EEEEE|]
testInput6 =
  [r|AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA|]

--
-- EEEEE
-- EXXXX
-- EEEEE
-- EXXXX
-- EEEEE

run :: String -> IO ()
run input = void $ do
  -- input <- putStrLn "#####    testInput   #####" >> pure testInput3
  print input
  let parsed = parse input
  -- mprint parsed
  let resA = solveA parsed
  print resA

  -- resA @=? 1375476

  let resB = solveB parsed
  resB

  print $ isNeigh (V2 0 1) (V2 0 0)
  print $ isNeigh (V2 0 1) (V2 0 1)
  print $ isNeigh (V2 0 0) (V2 1 1)
  print $ isNeigh (V2 1 1) (V2 0 1)
  print $ isNeigh (V2 1 1) (V2 (-1) 1)

-- print $ splitGroup [(Nothing, V2 1 0), (Nothing, V2 1 0), (Nothing, V2 1 0)]

-- resA @=? 1715
-- let resB = solveB parsed
-- resB

-- resB @=? 1739
