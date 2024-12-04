module Day.Day04 (run) where

import Control.Arrow ((>>>))
import Control.Lens ((&))
import Control.Monad (void)
import Data.List (isPrefixOf, tails, transpose)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Linear (V2 (V2))
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Text.Read (readMaybe)
import Utils (countP, diagonals, parseAsciiMap)

solveA :: String -> Int
solveA (lines -> ls) = allTails & countP isOK
 where
  isOK s = "XMAS" `isPrefixOf` s || "SAMX" `isPrefixOf` s
  allTails =
    concatMap
      (concatMap tails)
      [ ls
      , transpose ls
      , diagonals ls
      , diagonals $ reverse ls
      ]

data Xmas = X | M | A | S deriving (Eq, Read)

solveB :: [Char] -> Int
solveB ls = grid & Map.filter (== A) & Map.keys & countP check
 where
  grid = parseAsciiMap ((: []) >>> readMaybe @Xmas) ls
  check = getPoses >>> map (mapMaybe (grid Map.!?)) >>> all okRow

  okRow :: [Xmas] -> Bool
  okRow [M, S] = True
  okRow [S, M] = True
  okRow _ = False

  getPoses p = [[p + 1, p - 1], [p + (V2 1 (-1)), p + (V2 (-1) 1)]]

run :: String -> IO ()
run input = void $ do
  let resA = solveA input
  print resA
  resA @=? 2591
  let resB = solveB input
  print resB
  resB @=? 1880