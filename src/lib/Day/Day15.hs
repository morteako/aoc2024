module Day.Day15 (run) where

import Control.Arrow ((>>>))
import Control.Lens hiding (Empty)
import Control.Monad (void)
import Data.Foldable (for_)
import Data.Foldable.Extra (sumOn')
import Data.List
import Data.List.Split
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Linear
import Print
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils

data BoxDir = BL | BR deriving (Eq)

instance Show BoxDir where
  show BL = "["
  show BR = "]"

instance Show BB where
  show BB = "O"

data BB = BB deriving (Eq)

data Tile a = Border | Box a | Empty | Robot deriving (Eq)

data Dir = L | U | R | D deriving (Show, Eq)

instance (Show a) => Show (Tile a) where
  show Border = "#"
  show Empty = "."
  show (Box x) = show x
  show Robot = "@"

parse :: [Char] -> (Map.Map (V2 Int) (Tile BB), [Dir])
parse = splitOn "\n\n" >>> (\[grid, path] -> (parseAsciiMap f grid, mapMaybe g path))
 where
  f '#' = Just $ Border
  f '@' = Just $ Robot
  f 'O' = Just $ Box BB
  f '.' = Just $ Empty
  f x = error (show x)

  g '<' = Just L
  g '^' = Just U
  g '>' = Just R
  g 'v' = Just D
  g x = Nothing

move x L = x + V2 (-1) 0
move x R = x + V2 1 0
move x U = x + V2 0 (-1)
move x D = x + V2 0 1

tryWalk :: (Ord a, Show a) => Map.Map a (Tile BB) -> Tile BB -> [a] -> Maybe (Map.Map a (Tile BB))
tryWalk grid prev (p : ps) =
  case Map.lookup p grid of
    Nothing -> Nothing
    Just Border -> Nothing
    Just Empty -> Just $ Map.insert p prev grid
    Just (Box BB) -> tryWalk (Map.insert p prev grid) (Box BB) ps
    Just Robot -> error $ show (p, grid)

-- walk grid p [] = grid
walk grid p d =
  case tryWalk grid Robot (drop 1 $ iterate (flip move d) p) of
    Nothing -> (p, grid)
    Just newGrid -> (move p d, Map.insert p Empty newGrid)

iterateWalks grid p [d] = let (pp, ggrid) = walk grid p d in [(ggrid, pp)]
iterateWalks grid p (d : dirs) = let (pp, ggrid) = walk grid p d in (ggrid, pp) : iterateWalks ggrid pp dirs

calcGps (V2 x y) = x + 100 * y

solveA :: (Map.Map (V2 Int) (Tile BB), [Dir]) -> IO ()
solveA (startGrid, path) = do
  let robotStart = Map.filter (== Robot) startGrid & Map.keys & head

  let resGrids = iterateWalks startGrid robotStart path

  mprint startGrid
  printMap $ startGrid

  -- for_ (zip path resGrids) $ \(d, (g, p)) -> do
  --   print $ "After " ++ show d
  --   printMap g

  let (resG, _) = last resGrids

  let gpsScores =
        resG
          & Map.filter (== Box BB)
          & Map.keys
          & sumOn' calcGps

  print gpsScores

(##) = (,)

widenMap :: Map.Map (V2 Int) (Tile BB) -> Map.Map (V2 Int) (Tile BoxDir)
widenMap =
  Map.foldMapWithKey
    ( \p t -> case t of
        Empty -> f p [Empty, Empty]
        Border -> f p [Border, Border]
        Robot -> f p [Robot, Empty]
        Box BB -> f p [Box BL, Box BR]
    )
 where
  f (V2 x y) [a, b] = Map.fromList [V2 (x * 2) y ## a, V2 (x * 2 + 1) y ## b]

tryWalkB :: Map.Map (V2 Int) (Tile BoxDir) -> Tile BoxDir -> Dir -> V2 Int -> Maybe (Map.Map (V2 Int) (Tile BoxDir))
tryWalkB grid prev dir p =
  case Map.lookup p grid of
    Nothing -> Nothing
    Just Border -> Nothing
    Just Empty -> Just $ Map.insert p prev grid
    Just (Box boxDir) | elem dir [U, D] -> do
      let boxGrid = expandGroup dir grid (Map.singleton p $ Box boxDir)
      let allMoved = boxGrid & Map.keys & map (flip move dir)

      let newGrid = grid & Map.insert p prev -- ALWAYS ROBOT?
      -- & Map.unionWith (Map.f
      if all (\cp -> Map.member cp boxGrid || Map.lookup cp grid == Just Empty) allMoved
        then Nothing -- TODO
        else Nothing
    Just (Box boxDir) | otherwise -> tryWalkB (Map.insert p prev grid) (Box boxDir) dir (move p dir)
    Just Robot ->
      error "wtfrobot"

-- case Map.lookup (move p dir) grid of
--   Just (Box boxDir) | elem dir [U, D] -> tryWalkTwo grid dir (getBoxDirps p boxDir) -- & fmap (Map.insert p Empty)
--   _ -> tryWalkB grid Robot dir (move p dir)

getBoxDirps (V2 x y) BR = (V2 (x - 1) y, V2 x y)
getBoxDirps (V2 x y) BL = (V2 x y, V2 (x + 1) y)

-- tryWalkTwo :: Map.Map (V2 Int) (Tile BoxDir) -> Dir -> (V2 Int, V2 Int) -> Maybe (Map.Map (V2 Int) (Tile BoxDir))
-- tryWalkTwo grid dir (pl, pr) =
--   case (Map.lookup pl grid, Map.lookup pr grid) of
--     (Just (Box BL), Just (Box BR)) -> tryWalkTwo (grid & Map.insert pl (Box BL) & Map.insert pr (Box BR)) dir (move pl dir, move pr dir)
--     (Just Empty, Just Empty) -> Just (grid & Map.insert pl (Box BL) & Map.insert pr (Box BR))
--     (Just Robot, _) -> error "Wtf"
--     (_, Just Robot) -> error "Wtf"
--     _ -> Nothing

walkB :: Map.Map (V2 Int) (Tile BoxDir) -> V2 Int -> Dir -> (V2 Int, Map.Map (V2 Int) (Tile BoxDir))
walkB grid p d =
  case tryWalkB grid Robot d (move p d) of
    Nothing -> (p, grid)
    Just newGrid -> (move p d, Map.insert p Empty newGrid)

iterateWalksB grid p [d] = let (pp, ggrid) = walkB grid p d in [(ggrid, pp)]
iterateWalksB grid p (d : dirs) = let (pp, ggrid) = walkB grid p d in (ggrid, pp) : iterateWalksB ggrid pp dirs

type WideGrid = Map.Map (V2 Int) (Tile BoxDir)

expandGroup :: Dir -> WideGrid -> WideGrid -> WideGrid
expandGroup dir grid curs = do
  let valids = curs <> Map.foldMapWithKey (\k _ -> f k) curs
  if Map.size valids > Map.size curs
    then
      expandGroup dir grid valids
    else
      curs
 where
  f p = case Map.lookup (move p dir) grid of
    Nothing -> Map.empty
    Just Border -> Map.empty
    Just Empty -> Map.empty
    Just (Box boxDir) -> let (a, b) = getBoxDirps p boxDir in Map.fromList [a ## Box BR, b ## Box BL]
    Just Robot -> Map.empty

solveB :: (Map.Map (V2 Int) (Tile BB), [Dir]) -> IO ()
solveB (widenMap -> startGrid, path) = do
  printMap startGrid

  print "qq"

  let robotStart = Map.filter (== Robot) startGrid & Map.keys & head

  let resGrids = iterateWalksB startGrid robotStart path

  mprint startGrid
  printMap $ startGrid

  for_ (zip path resGrids) $ \(d, (g, p)) -> do
    print $ "After " ++ show d
    printMap g

-- printMap $ resGrid

printMap :: (Show a) => Map.Map (V2 Int) (Tile a) -> IO ()
printMap = fmap (show >>> head) >>> printV2MapC

testInput =
  [r|########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
|]

testInput2 =
  [r|##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^|]

testInput3 =
  [r|#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^|]

run :: String -> IO ()
run input = void $ do
  input <- putStrLn "#####    testInput   #####" >> pure testInput3
  -- print input
  let parsed = parse input
  -- mprint parsed
  -- let parsedB = parseWiden input

  -- solveA parsed

  solveB parsed

-- let resA = solveA parsed
-- mprint resA

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
