module Day.Day03 (run) where

import Control.Arrow ((>>>))
import Control.Lens (to, (^..))
import Control.Lens.Regex.Text (match, regex)
import Control.Monad (void)

import Data.List (foldl', stripPrefix)
import Data.List.Extra (sumOn')
import Data.Text qualified as T
import Test.HUnit ((@=?))

data Op = Mul Int Int | Do | Dont

parse :: String -> _
parse (T.pack -> txt) = (txt ^.. [regex|mul\(\d+,\d+\)|do\(\)|don't\(\)|] . match . to (T.unpack >>> parseToOp))
 where
  parseToOp "do()" = Do
  parseToOp "don't()" = Dont
  parseToOp (stripPrefix "mul" -> Just ints) = uncurry Mul (read ints)

solveA :: [Op] -> Int
solveA =
  sumOn'
    ( \case
        Mul a b -> a * b
        _ -> 0
    )

solveB :: [Op] -> Int
solveB = foldl' f (True, 0) >>> snd
 where
  f (b, s) Do = (True, s)
  f (b, s) Dont = (False, s)
  f (b, s) (Mul x y) = (b, if b then s + (x * y) else s)

run :: String -> IO ()
run input = void $ do
  let parsed = parse input
  let resA = solveA parsed
  print resA
  resA @=? 153469856
  let resB = solveB parsed
  print resB
  resB @=? 77055967
