module Day.Day20 (run) where

import Control.Lens (ifind)
import Control.Monad (guard, void)
import Data.Graph.AStar (aStar)
import Data.HashSet qualified as HashSet
import Data.List (group, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Linear (V2 (..))
import Test.HUnit ((@=?))
import Utils (parseAsciiMap)

pattern Tile = '.'
pattern Wall = '#'
pattern Start = 'S'
pattern End = 'E'

parse :: String -> Map (V2 Int) Char
parse = parseAsciiMap Just

solveBoth :: Map (V2 Int) Char -> (Int, Int)
solveBoth grid = do
  let Just (start, _) = ifind (\_ -> (== Start)) grid
  let Just (end, _) = ifind (\_ -> (== End)) grid

  let isGoal = (== end)
  let neighDist a b = 1
  let heur t = sum $ abs $ end - t
  let neighs (V2 x y) = HashSet.fromList $ do
        v@(V2 xx yy) <- [V2 (succ x) y, V2 (pred x) y, V2 x (succ y), V2 x (pred y)] :: [V2 Int]
        let look = grid Map.!? v
        guard $ look /= Nothing && look /= Just Wall
        pure v

  let Just ((start :) -> res) = aStar neighs neighDist heur isGoal start

  let normalTime = length res - 1

  let posToTimeLeftList = zip (reverse res) [0 ..]

  let posToTimeUsedList = zip res [0 ..]

  let dist a b = (sum $ abs $ a - b)

  let minTarget = 100

  let posesWithDistsMax20 limit = sum $ map length $ group $ sort $ do
        (p, timeUsed) <- posToTimeUsedList
        (p2, timeLeft) <- posToTimeLeftList

        let reachDist = dist p p2
        guard $ reachDist <= limit

        let newTime = timeUsed + timeLeft + reachDist

        let diff = normalTime - newTime
        guard $ diff >= minTarget
        pure diff

  let resA = posesWithDistsMax20 2
  let resB = posesWithDistsMax20 20
  (resA, resB)

run :: String -> IO ()
run input = void $ do
  let parsed = parse input
  let (resA, resB) = solveBoth parsed

  print resA
  resA @=? 1426

  print resB
  resB @=? 1000697