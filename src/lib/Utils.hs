module Utils where

import Control.Lens
import Data.Foldable (Foldable (foldl'))
import Data.List.Extra hiding (foldl1')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (Sum (Sum, getSum))
import Debug.Trace
import GHC.Base (Semigroup)
import Linear (V2 (V2))
import Text.Read (readMaybe)

occurences :: (Ord a) => [a] -> Map a Int
occurences xs = Map.fromListWith (+) $ map (,1) xs

zipWithNext :: (a -> a -> b) -> [a] -> [b]
zipWithNext f xs = zipWith f xs (tail xs)

toTuple :: [b] -> (b, b)
toTuple [x, y] = (x, y)

pairs :: [a] -> [(a, a)]
pairs = zip <*> tail

charRead :: (Read s) => Char -> s
charRead = read . pure

charReadMaybe :: (Read s) => Char -> Maybe s
charReadMaybe = readMaybe . pure

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

printV2MapC :: (Ord k) => Map.Map (V2 k) Char -> IO ()
printV2MapC m = do
    putStrLn "--------"
    let xs = Map.toList m
    let g = groupOn (\(V2 _ y, _) -> y) $ sortOn (\(V2 x y, _) -> V2 y x) xs
    let gg = fmap (fmap snd) g
    mapM_
        ( \x -> do
            mapM_ (putChar) x
            putStrLn ""
        )
        $ gg
    putStrLn ""

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

-- makeMap :: [Dot] -> Map.Map (V2 Int) [Label]
-- makeMap = Map.unionWith (<>) defMap . Map.fromListWith (<>) . fmap (\(Dot l pos) -> (pos, pure l))

-- printMap :: Map.Map (V2 Int) [Label] -> IO ()
-- printMap m = do
--   putStrLn "--------"
--   let xs = Map.toList $ Map.mapKeys (\(V2 x y) -> V2 y x) m
--   let g = groupOn (\(V2 x y, _) -> x) xs
--   let gg = reverse $ (fmap . concatMap) (fp . snd) g

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

apply1 :: (a -> b) -> [a] -> b
apply1 f [a] = f a
apply1 f xs = error $ "apply1: " ++ show (fmap (const ()) xs)

apply2 :: (a -> a -> b) -> [a] -> b
apply2 f [a, b] = f a b
apply2 f xs = error $ "apply2: " ++ show (fmap (const ()) xs)

apply3 :: (a -> a -> a -> b) -> [a] -> b
apply3 f [a, b, c] = f a b c
apply3 f xs = error $ "apply3: " ++ show (fmap (const ()) xs)
