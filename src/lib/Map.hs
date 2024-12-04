module Map where

import Data.Map (Map)
import Data.Map qualified as Map

find :: (Ord k) => k -> Map k v -> v
find = flip (Map.!)

find_ :: (Ord k) => Map k v -> k -> v
find_ = (Map.!)

lookup_ :: (Ord k) => Map k v -> k -> v
lookup_ = (Map.!)

fromListIndexed :: (Ord a, Enum a, Num a) => [v] -> Map a v
fromListIndexed vs = Map.fromList (zip [0 ..] vs)

fromListOn :: (Ord k) => (k -> v) -> [k] -> Map k v
fromListOn f = fromListOnWith f (error "duplicates in fromListOn")

fromListOnWith :: (Ord k) => (k -> v) -> (v -> v -> v) -> [k] -> Map k v
fromListOnWith f comb = Map.fromListWith comb . map (\k -> (k, f k))