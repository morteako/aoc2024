{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils where

import Control.Lens
import Data.Foldable (Foldable (foldl'))
import Data.Kind
import Data.List.Extra hiding (foldl1')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (Sum (Sum, getSum))
import Debug.Trace
import GHC.Base (Semigroup)
import Linear (V2 (V2))

occurences :: (Ord a) => [a] -> Map a Int
occurences xs = Map.fromListWith (+) $ map (,1) xs

zipWithNext :: (a -> a -> b) -> [a] -> [b]
zipWithNext f xs = zipWith f xs (tail xs)

pairs :: [a] -> [(a, a)]
pairs = zip <*> tail

readInt :: String -> Int
readInt = read

(?:) :: Maybe c -> c -> c
(?:) = flip fromMaybe

(=:) :: a -> b -> (a, b)
(=:) = (,)

count :: (Eq a) => a -> [a] -> Int
count x = getSum . foldMap (Sum . fromEnum . (== x))

countP :: (Foldable f) => (a -> Bool) -> f a -> Int
countP p = getSum . foldMap (Sum . fromEnum . p)

semiFoldMapl' :: (Semigroup v, Functor f, Foldable f) => (a -> v) -> f a -> v
semiFoldMapl' av = foldl1' (<>) . fmap av

semiFoldMapr :: (Semigroup v, Functor f, Foldable f) => (a -> v) -> f a -> v
semiFoldMapr av = foldr1 (<>) . fmap av

foldl1' :: (Foldable t) => (a -> a -> a) -> t a -> a
foldl1' f xs =
    fromMaybe
        (errorWithoutStackTrace "foldl1': empty structure")
        (foldl' mf Nothing xs)
  where
    mf m y =
        Just
            ( case m of
                Nothing -> y
                Just x -> f x y
            )

printV2Map :: (Show a, Ord k) => Map.Map (V2 k) a -> IO ()
printV2Map m = do
    putStrLn "--------"
    let xs = Map.toList m
    let g = groupOn (\(V2 _ y, _) -> y) $ sortOn (\(V2 x y, _) -> V2 y x) xs
    let gg = fmap (fmap snd) g
    mapM_
        ( \x -> do
            mapM_ (putStr . show) x
            putStrLn ""
        )
        $ gg
    putStrLn ""

traceLab :: (Show a) => [Char] -> a -> a
traceLab s x = trace (s ++ ": " ++ show x) x

traceOn :: (Show a) => (a -> [Char]) -> a -> a
traceOn f x = trace (f x) x

(.?) :: (Show t1) => (t2 -> t1) -> (t1 -> t3) -> t2 -> t3
(.?) f g = \x -> g $ traceShowId (f x)

parseAsciiMap ::
    (Char -> Maybe a) ->
    String ->
    Map (V2 Int) a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton
  where
    asciiGrid :: IndexedFold (V2 Int) String Char
    asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

-- printing

printlab :: (Show a) => [Char] -> a -> IO ()
printlab s a = putStr (s ++ " ") >> print a

class Print a where
    mprint :: a -> IO ()

instance Print String where
    mprint = print

instance {-# INCOHERENT #-} (Show a) => Print [a] where
    mprint = mapM_ print

instance {-# INCOHERENT #-} (Show a) => Print a where
    mprint = print

instance {-# INCOHERENT #-} (Show k, Show v) => Print (Map k v) where
    mprint = mapM_ (putStrLn . (\(k, v) -> show k ++ " => " ++ show v)) . Map.toList

-- makeMap :: [Dot] -> Map.Map (V2 Int) [Label]
-- makeMap = Map.unionWith (<>) defMap . Map.fromListWith (<>) . fmap (\(Dot l pos) -> (pos, pure l))

-- defMap = Map.insertWith (++) 0 [T] $ Map.fromList $ fmap (,[]) xs
--  where
--   xs = V2 <$> [-11 .. 14] <*> [-5 .. 15]

--   limU = 5
--   limD = 5

-- fp x = case x of
--   [] -> "."
--   [T] -> "s"
--   labs -> show $ minimum labs

-- printMap :: Map.Map (V2 Int) [Label] -> IO ()
-- printMap m = do
--   putStrLn "--------"
--   let xs = Map.toList $ Map.mapKeys (\(V2 x y) -> V2 y x) m
--   let g = groupOn (\(V2 x y, _) -> x) xs
--   let gg = reverse $ (fmap . concatMap) (fp . snd) g

--   mapM_ (print . sort) $ Map.filter (\x -> length x > 1) m
--   mapM_ putStrLn gg
--   putStrLn ""

-- Copied from universe-base
diagonals :: [[a]] -> [[a]]
diagonals = tail . go []
  where
    go b es_ =
        [h | h : _ <- b] : case es_ of
            [] -> transpose ts
            e : es -> go (e : ts) es
      where
        ts = [t | _ : t <- b]
