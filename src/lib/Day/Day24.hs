module Day.Day24 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (void)
import Data.Digits (digits, unDigits)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Print (Print (mprint), printlab)
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)

import Data.Foldable (for_)
import Utils (apply2, count, readInt)

data Ops = AND | XOR | OR deriving (Show, Read, Eq, Ord)

data Op = Op {l :: String, op :: Ops, r :: String, to :: String} deriving (Eq, Ord)
instance Show Op where
  show op = op.l ++ " " ++ show op.op ++ " " ++ op.r ++ " -> " ++ op.to

parse :: String -> (Map.Map String Bool, [Op])
parse = lines >>> splitOn [[]] >>> apply2 (,) >>> f
 where
  f (startVals, eqs) = do
    let m = Map.fromList $ map parseStartVal startVals
    let r = map parseOp eqs
    (m, r)

  parseStartVal (splitOn ": " -> [k, v]) = (k, readInt v == 1)
  parseOp (words -> [l, op, r, "->", to]) = Op l (read op) r to

eval :: Ops -> Bool -> Bool -> Bool
eval AND = (&&)
eval OR = (||)
eval XOR = (/=)

solveA :: (Map.Map String Bool, [Op]) -> IO ()
solveA (inits, eqs) = do
  let ops = Map.fromList $ map (\x -> (x.to, Right x)) eqs

  let get cur s = case Map.lookup s cur of
        Nothing -> error $ "hmm " ++ s
        Just (Left j) -> j
        Just (Right op) -> eval op.op (get cur op.l) (get cur op.r)

  let combs = Map.unionWith undefined (fmap Left inits) ops

  let evalE c (Left i) = i
      evalE c (Right s) = get c s

  let zs = Map.filterWithKey (\k _ -> head k == 'z') combs & Map.mapWithKey (\k _ -> get combs k) & Map.elems
  print $ unDigits 2 $ reverse $ map fromEnum zs

  -- chaotic part 2 code following :)) :

  let xs = Map.filterWithKey (\k _ -> head k == 'x') inits & Map.elems & fmap fromEnum
  printlab "xs   " xs
  let ys = Map.filterWithKey (\k _ -> head k == 'y') inits & Map.elems & fmap fromEnum
  printlab "ys   " ys
  printlab "added" $ addDigits xs ys
  printlab "zs   " $ map fromEnum zs
  let countNumOnesFail = count True $ zs

  let onlyZs = combs & Map.keys & filter (\k -> head k == 'z') & sort

  let renames = mapMaybe getRename eqs & Map.fromListWith (\a b -> error $ show (a, b))

  let getRename x = Map.findWithDefault x x renames

  let rename op = Op (getRename op.l) op.op (getRename op.r) (getRename op.to)

  let ops = Map.fromList $ map (\x -> (x.to, Right x)) (map rename eqs)

  let combs = Map.unionWith (\a b -> error $ show (a, b)) (fmap Left inits) ops

  let getRels cur = case Map.lookup cur combs of
        Nothing -> error $ cur
        Just (Left i) -> []
        Just (Right op) -> sort $ op : (getRels op.l ++ getRels op.r)

  let getUniqRels _ [] = []
      getUniqRels prevs (cur : rest) = let c = Set.fromList (getRels cur) `Set.difference` prevs in Set.toList c : getUniqRels (prevs <> c) rest

  for_ (getUniqRels Set.empty onlyZs) $ \rels -> do
    mprint rels
    print $ count AND $ map (.op) rels
    print $ count XOR $ map (.op) rels
    print "--------------------"

getRename op | head op.to == 'z' = Nothing
getRename op = case (op.l, op.r) of
  ('x' : nn, 'y' : mm) | nn == mm -> Just $ swap (nn ++ show op.op, op.to)
  ('y' : nn, 'x' : mm) | nn == mm -> Just $ swap (nn ++ show op.op, op.to)
  _ -> Nothing

addDigits (reverse -> x) (reverse -> y) = reverse $ digits 2 $ unDigits 2 x + unDigits 2 y

run :: String -> IO ()
run input = void $ do
  let parsed = parse input
  let resA = solveA parsed
  resA
