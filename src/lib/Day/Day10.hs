module Day.Day10 (run) where

import Control.Arrow ((>>>))
import Control.Lens ((&))
import Control.Monad (void)
import Data.Foldable.Extra (sumOn')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Linear (V2 (V2))
import Test.HUnit ((@=?))
import Utils (charReadMaybe, parseAsciiMap)

parse :: String -> Map (V2 Int) Int
parse = parseAsciiMap (\x -> if x == '.' then Just 9999 else charReadMaybe x)

findTrailtops :: ([V2 Int] -> Int) -> Map (V2 Int) Int -> Int
findTrailtops count grid = map (walk 0) starts & sumOn' count
 where
  starts = Map.filter (== 0) grid & Map.keys

  walk 9 pos = [pos]
  walk v pos =
    nexts pos
      & concatMap f
   where
    f p =
      case Map.lookup p grid of
        Just lvl | lvl == v + 1 -> walk lvl p
        _ -> []

nexts :: (Num a) => V2 a -> [V2 a]
nexts pos = [pos + v | v <- [V2 0 1, V2 1 0, V2 (-1) 0, V2 0 (-1)]]

run :: String -> IO ()
run input = void $ do
  let parsed = parse input

  let resA = findTrailtops (Set.fromList >>> Set.size) parsed
  print resA
  resA @=? 760

  let resB = findTrailtops length parsed
  print resB
  resB @=? 1764
