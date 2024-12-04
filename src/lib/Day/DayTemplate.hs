module Day.DayTemplate (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (void)
import Data.List
import Data.Maybe
import Print
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils

parse = id

solveA = id

solveB = id

testInput =
  [r|10000
1000
|]

run :: String -> IO ()
run input = void $ do
  input <- putStrLn "#####    testInput   #####" >> pure testInput
  print input
  let parsed = parse input
  mprint parsed
  let resA = solveA parsed
  mprint resA

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
