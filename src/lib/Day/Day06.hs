module Day.Day06 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (unless, void, when)
import Data.List.Extra (splitOn)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Linear
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils (parseAsciiMap)

data Tile = Floor | Block deriving (Show, Eq)

parse :: String -> Map.Map (V2 Int) (Maybe Tile)
parse =
  parseAsciiMap
    ( \case
        '.' -> Just $ Just Floor
        '#' -> Just $ Just Block
        '^' -> Just Nothing
    )

data Dir = DUp | DRight | DDown | DLeft deriving (Eq, Ord, Enum, Show)

suc DLeft = DUp
suc x = succ x

next p DUp = p + V2 0 (-1)
next p DDown = p + V2 0 (1)
next p DRight = p + V2 1 0
next p DLeft = p + V2 (-1) 0

nextStep p dir Floor = (next p dir, dir)
nextStep p dir Block = let nextDir = suc dir in (p, nextDir)

splitMap m = (startPos, Map.insert startPos Floor grid)
 where
  ([startPos], grid) = Map.mapEither (maybe (Left ()) Right) m & over _1 Map.keys

walk grid (pos, dir) = case Map.lookup (next pos dir) grid of
  Nothing -> [(pos, dir)]
  Just tile -> (pos, dir) : walk grid (nextStep pos dir tile)

solveA :: (Ord a, Num a) => V2 a -> Map.Map (V2 a) Tile -> Int
solveA startPos grid = res & Set.size -- Map.mapWithKey (\k c -> if c == Block then '#' else if Set.member k res then 'O' else '.') grid
 where
  res = walk grid (startPos, DUp) & map fst & Set.fromList

getPath :: (Ord a, Num a) => V2 a -> Map.Map (V2 a) Tile -> _
getPath startPos grid = res
 where
  res = walk grid (startPos, DUp)

solveB startPos grid =
  foldMap (\((p, d), past) -> turnAndCheck (p, d) past) zipped
    & Set.delete startPos
    & Set.size
 where
  path = walk grid (startPos, DUp)

  zipped = zip path (scanl (flip Set.insert) mempty $ map fst path)

  turnAndCheck (p, d) past
    | Set.member blockPos past = mempty
    | grid Map.!? blockPos == Just Block = mempty
    | grid Map.!? blockPos == Nothing = mempty
    | checkLoop (Set.empty) (nextStep p d Block) = Set.singleton blockPos
    | otherwise = mempty
   where
    blockPos = next p d
    newGrid = Map.insert blockPos Block grid

    checkLoop vis (pos, dir) = case Map.lookup (next pos dir) newGrid of
      _ | Set.member (pos, dir) vis -> True
      Nothing -> False
      Just t -> checkLoop (Set.insert (pos, dir) vis) $ nextStep pos dir t

run :: String -> IO ()
run input = void $ do
  let (startPos, grid) = parse input & splitMap

  let resA = solveA startPos grid
  resA @=? 5131
  print resA

  let resB = solveB startPos grid
  print resB
  resB @=? 1784