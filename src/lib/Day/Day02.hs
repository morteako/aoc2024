module Day.Day02 (run) where

import Control.Arrow ((>>>))
import Control.Lens ((&))
import Control.Monad (void)
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils (countP)

parse :: String -> [[Int]]
parse = lines >>> map (words >>> map read)

levelsOk :: [Int] -> Bool
levelsOk = (pairs >>> (\xs -> all okAsc xs || all okDesc xs))
 where
  okAsc (a, b) = b - a `elem` [1, 2, 3]
  okDesc (a, b) = a - b `elem` [1, 2, 3]

solveA :: [[Int]] -> Int
solveA = countP levelsOk

solveB :: [[Int]] -> Int
solveB = countP (getOneRemovedLists >>> any levelsOk)

getOneRemovedLists :: [b] -> [[b]]
getOneRemovedLists (zip [0 ..] -> xs) = map (\(i, _) -> filter (\(ii, _) -> i /= ii) xs & map snd) xs

pairs :: [a] -> [(a, a)]
pairs = zip <*> tail

run :: String -> IO ()
run input = void $ do
  let parsed = parse input
  let resA = solveA parsed
  print resA
  resA @=? 585
  let resB = solveB parsed
  print resB
  resB @=? 626
