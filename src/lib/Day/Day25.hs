module Day.Day25 (run) where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow ((>>>))
import Control.Monad (guard, void)
import Data.Either (partitionEithers)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils (count, countP)

pattern Fill = '#'

parse :: String -> ([[Int]], [[Int]])
parse = lines >>> splitOn [[]] >>> mapMaybe f >>> partitionEithers
 where
  f xs = fmap Left (parseKeyOrLock xs) <|> fmap Right (parseKeyOrLock (reverse xs))
  parseKeyOrLock (first : rest)
    | all (== Fill) first = Just $ map (count Fill) $ transpose rest
    | otherwise = Nothing

countOverlaps :: [[Int]] -> [[Int]] -> Int
countOverlaps = (countP (all (<= 5) . uncurry (zipWith (+))) .) . liftA2 (,)

run :: String -> IO ()
run input = void $ do
  let resA = uncurry countOverlaps $ parse input
  print resA
  resA @=? 3365
