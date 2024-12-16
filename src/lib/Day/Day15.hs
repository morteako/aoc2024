module Day.Day15 (run) where

import Control.Arrow ((>>>))
import Control.Lens (iall, (&))
import Control.Monad (void)
import Data.Foldable.Extra (sumOn')
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Linear (V2 (..))
import Test.HUnit ((@=?))
import Utils qualified

data WideBox = BL | BR deriving (Eq)

data SimpleBox = SimpleBox deriving (Eq)

data Tile a = Border | Box a | Empty | Robot deriving (Eq)

data Dir = L | U | R | D deriving (Eq)

parse :: [Char] -> (Grid SimpleBox, [Dir])
parse = splitOn "\n\n" >>> (\[grid, path] -> (Utils.parseAsciiMap f grid, mapMaybe g path))
 where
  f '#' = Just $ Border
  f '@' = Just $ Robot
  f 'O' = Just $ Box SimpleBox
  f '.' = Just $ Empty
  f x = error (show x)

  g '<' = Just L
  g '^' = Just U
  g '>' = Just R
  g 'v' = Just D
  g x = Nothing

move :: V2 Int -> Dir -> V2 Int
move x L = x + V2 (-1) 0
move x R = x + V2 1 0
move x U = x + V2 0 (-1)
move x D = x + V2 0 1

(##) = (,)

type Grid boxDir = Map.Map (V2 Int) (Tile boxDir)

widenMap :: Grid SimpleBox -> Grid WideBox
widenMap =
  Map.foldMapWithKey
    ( \p t -> case t of
        Empty -> f p (Empty, Empty)
        Border -> f p (Border, Border)
        Robot -> f p (Robot, Empty)
        Box SimpleBox -> f p (Box BL, Box BR)
    )
 where
  f (V2 x y) (a, b) = Map.fromList [V2 (x * 2) y ## a, V2 (x * 2 + 1) y ## b]

calcGps :: (Num a) => V2 a -> a
calcGps (V2 x y) = x + 100 * y

walk :: forall boxDir. (Eq boxDir) => (boxDir -> V2 Int -> Map.Map (V2 Int) (Tile boxDir)) -> (V2 Int, Grid boxDir) -> Dir -> (V2 Int, Grid boxDir)
walk boxF (pos, grid) dir =
  case tryWalk grid (move pos dir) of
    Nothing -> (pos, grid)
    Just newGrid -> (move pos dir, Map.insert pos Empty newGrid)
 where
  tryWalk :: Grid boxDir -> V2 Int -> Maybe (Grid boxDir)
  tryWalk grid p =
    case Map.lookup p grid of
      Nothing -> Nothing
      Just Border -> Nothing
      Just Empty -> Just $ Map.insert p Robot grid
      Just (Box boxDir) -> do
        let boxGrid = findAllBoxesInDir boxF dir grid (Map.singleton p $ Box boxDir)
        let allMoved = boxGrid & Map.mapKeys (flip move dir)
        let newEmpty = Map.difference boxGrid allMoved & fmap (const Empty)
        if iall (\pos _ -> Map.member pos boxGrid || Map.lookup pos grid == Just Empty) allMoved
          then Just $ Map.insert p Robot (newEmpty <> allMoved <> grid)
          else Nothing
      Just Robot ->
        error "bug :))"

findAllBoxesInDir :: (boxDir -> V2 Int -> Grid boxDir) -> Dir -> Grid boxDir -> Grid boxDir -> Grid boxDir
findAllBoxesInDir getBoxes dir grid curs = do
  let widen = Map.foldMapWithKey (\pos _ -> getBox pos) curs
  let news = curs <> widen <> Map.foldMapWithKey (\pos _ -> getBox (move pos dir)) widen
  if Map.size news > Map.size curs
    then
      findAllBoxesInDir getBoxes dir grid news
    else
      curs
 where
  getBox p = case Map.lookup p grid of
    Just (Box boxDir) -> getBoxes boxDir p
    Nothing -> Map.empty
    Just Border -> Map.empty
    Just Empty -> Map.empty
    Just Robot -> Map.empty

getNormalBoxes :: SimpleBox -> k -> Map.Map k (Tile SimpleBox)
getNormalBoxes SimpleBox p = Map.singleton p (Box SimpleBox)

getWidenedBoxes :: WideBox -> V2 Int -> Grid WideBox
getWidenedBoxes boxDir p = let (a, b) = getBoxPoses p boxDir in Map.fromList [a ## Box BL, b ## Box BR]
 where
  getBoxPoses (V2 x y) BR = (V2 (x - 1) y, V2 x y)
  getBoxPoses (V2 x y) BL = (V2 x y, V2 (x + 1) y)

solveA :: (Grid SimpleBox, [Dir]) -> Int
solveA (startGrid, path) = do
  let robotStart = Map.filter (== Robot) startGrid & Map.keys & head

  let (_, resG) = foldl' (walk getNormalBoxes) (robotStart, startGrid) path

  resG
    & Map.filter (\x -> x == Box SimpleBox)
    & Map.keys
    & sumOn' calcGps

solveB :: (Grid SimpleBox, [Dir]) -> Int
solveB (widenMap -> startGrid, path) = do
  let robotStart = Map.filter (== Robot) startGrid & Map.keys & head

  let (_, resG) = foldl' (walk getWidenedBoxes) (robotStart, startGrid) path

  resG
    & Map.filter (\x -> x == Box BL)
    & Map.keys
    & sumOn' calcGps

run :: String -> IO ()
run input = void $ do
  let parsed = parse input

  let resA = solveA parsed
  print resA
  resA @=? 1478649

  let resB = solveB parsed
  print resB

  resB @=? 1495455
