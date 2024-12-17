module Day.Day16 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad.Extra (guard, void)
import Data.Graph.Inductive (Graph (mkGraph), LPath (LP, unLPath))
import Data.Graph.Inductive.Query (sp, spLength, spTree)
import Data.Graph.Inductive.Tree (Gr)
import Data.IntSet qualified as Set
import Data.List (partition)
import Data.List.Extra (minimumOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Linear hiding (distance)
import Test.HUnit ((@=?))
import Utils qualified

parse :: [Char] -> (Map (V2 Int) Char, V2 Int, V2 Int)
parse str = (grid, start, end)
 where
  Just start = elemIndexOf ifolded ('S') grid
  Just end = elemIndexOf ifolded ('E') grid

  grid = Utils.parseAsciiMap f str

  f '.' = Just '.'
  f '#' = Nothing
  f 'S' = Just 'S'
  f 'E' = Just 'E'
  f xs = error $ show xs

data Dir = L | U | R | D
  deriving (Show, Eq, Ord)

data Point = Point {pos :: V2 Int, dir :: Dir} deriving (Show, Eq, Ord)

getDirs :: Dir -> [Dir]
getDirs L = [L, U, D]
getDirs R = [R, U, D]
getDirs U = [U, L, R]
getDirs D = [D, L, R]

getDirVec L = V2 (-1) 0
getDirVec R = V2 1 0
getDirVec U = V2 0 (-1)
getDirVec D = V2 0 1

distance :: Point -> Point -> Int
distance a b = if a.dir == b.dir then 1 else 1001

solveBoth :: (Map (V2 Int) Char, V2 Int, V2 Int) -> _
solveBoth (grid, start, end) = do
  let igrid :: Map (V2 Int, Dir) Int
      igrid = Map.fromList $ flip zip [0 ..] $ do
        p <- grid & Map.keys
        d <- [L, U, R, D]
        pure (p, d)
  let nodes = do
        (p, i) <- Map.toList igrid
        pure (i, p)

  let edges = do
        (i, (p, d)) <- nodes
        guard $ p /= end
        let forward = (p + getDirVec d, d, 1)
        let turns = do
              dir <- getDirs d
              pure (p, dir, if dir == d then 1 else 1000)
        (newP, newD, cost) <- forward : turns
        case igrid Map.!? (newP, newD) of
          Nothing -> []
          Just j -> pure (i, j, cost)

  let graph :: Gr _ _
      graph = mkGraph nodes edges

  let startNode = igrid Map.! (start, R)
  let endNodes = igrid & Map.filterWithKey (\k _ -> fst k == end) & Map.elems & Set.fromList

  let (costTarget, Just (Set.fromList -> pathSet), endNode) = minimumOn (view _1) $ flip mapMaybe (Set.toList endNodes) $ \endNode -> fmap (,sp startNode endNode graph,endNode) $ spLength startNode endNode graph

  let resA = costTarget

  let isRelevant (c, n, _) = c < costTarget && Set.notMember n pathSet

  let (nodesWithPotential, nodesWithoutPotential) =
        partition isRelevant $
          filter (\(n, _, _) -> Set.notMember n endNodes) $
            map (\((n, c) : restPath) -> (c, n, Set.fromList (n : map fst restPath))) $
              map unLPath $
                spTree startNode graph

  let getCost path = zipWith distance path (tail path) & sum

  let findAllBestPoints curs nots [] = curs
      findAllBestPoints curs nots ((c, n, path) : ns)
        | not (Set.disjoint path nots) = findAllBestPoints curs nots ns
        | c >= costTarget || Set.member n curs = findAllBestPoints curs nots ns
        | otherwise = case spLength n endNode graph of
            Just l
              | c + l == costTarget ->
                  let Just p = sp n endNode graph in findAllBestPoints (Set.union (path <> Set.fromList p) curs) nots ns
            Just l | c + l > costTarget -> findAllBestPoints curs (Set.insert n nots) ns
            Just l | c + l < costTarget -> error "wtf"
            Nothing -> findAllBestPoints curs nots ns

  let igridRev = igrid & Map.toList & map swap & Map.fromList

  let res = findAllBestPoints pathSet (Set.fromList $ filter (flip Set.notMember pathSet) $ map (view _2) nodesWithoutPotential) nodesWithPotential
  let resB = Map.size $ Map.fromList $ fmap (igridRev Map.!) $ Set.toList res
  (resA, resB)

run :: String -> IO ()
run input = void $ do
  let parsed = parse input
  let (resA, resB) = solveBoth parsed
  print resA
  resA @=? 147628
  print resB

-- resB @=? 498
