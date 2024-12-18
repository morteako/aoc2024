{-# LANGUAGE StrictData #-}

module Day.Day17 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (guard, void, when)
import Data.Bits (Bits (xor))
import Data.IntMap qualified as Map
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import Data.Word
import Debug.Trace hiding (traceShow)
import Debug.Trace qualified as Trace
import GHC.Records
import Map qualified
import Numeric (readOct, showHex, showIntAtBase, showOct)
import Print
import Test.HUnit ((@=?))
import Text.RawString.QQ (r)
import Utils

-- import Data.SBV

data Reg = A | B | C deriving (Show, Read, Eq, Ord)

data Op = Div Reg | Bxl | Bst | Jnz | Bxc | Out | Bdv deriving (Show)

data Oper = RegVal Reg | LitVal Int deriving (Show)

data Cmd = Cmd Op Oper deriving (Show)

parse :: String -> (Regs, [Cmd], [Int])
parse str = do
  let [regs, [prog]] = str & lines & splitOn [[]]

  let parseReg (words -> [_, take 1 -> regLetter, num]) = (read @Reg regLetter, read @Int num)

  let [a, b, c] = map parseReg regs & sort & fmap snd
  let regs = Regs a b c

  let parseOp [op, oper] =
        let
          q = case oper of
            _ | oper `isInfixOf` "0123" -> LitVal (read oper)
            "4" -> RegVal A
            "5" -> RegVal B
            "6" -> RegVal C
            _ -> LitVal (readInt oper)

          o = case op of
            "0" -> Div A
            "1" -> Bxl
            "2" -> Bst
            "3" -> Jnz
            "4" -> Bxc
            "5" -> Out
            "6" -> Div B
            "7" -> Div C
         in
          Cmd o q

  let parsedProg = drop 9 prog & splitOn "," & chunksOf 2 & map parseOp

  (regs, parsedProg, drop 9 prog & splitOn "," & map readInt)

data Regs = Regs {a :: Int, b :: Int, c :: Int} deriving (Show)

instance HasField "r" Regs (Reg -> Int) where
  getField :: Regs -> Reg -> Int
  getField regs reg = case reg of
    A -> regs.a
    B -> regs.b
    C -> error $ show regs.c

instance HasField "combo" Regs (Oper -> Int) where
  getField :: Regs -> Oper -> Int
  getField regs reg = case reg of
    RegVal a -> regs.r a
    LitVal a -> a

instance HasField "set" Regs (Reg -> Int -> Regs) where
  getField :: Regs -> Reg -> Int -> Regs
  getField regs reg v = case reg of
    A -> regs{a = v}
    B -> regs{b = v}
    C -> regs{c = v}

instance HasField "inc" Int Int where
  getField :: Int -> Int
  getField i = i + 1

instance HasField "dec" Int Int where
  getField :: Int -> Int
  getField i = i - 1

instance HasField "lit" Oper Int where
  getField :: Oper -> Int
  getField (LitVal x) = x
  getField (RegVal A) = 4
  getField (RegVal B) = 5
  getField (RegVal C) = 6

doAll cmds regs pointer =
  case Map.lookup pointer cmds of
    Nothing -> []
    Just cmd ->
      let r@(!p, !rs, !output) = doOne regs pointer cmd
       in r : doAll cmds rs p

doOne :: Regs -> Int -> Cmd -> _
doOne (id -> regs) pointer (Cmd op oper) =
  case op of
    Jnz ->
      if regs.a == 0
        then (pointer.inc, regs, Nothing)
        else (oper.lit, regs, Nothing)
    Div r ->
      (pointer.inc, regs.set r (regs.r A `div` (2 ^ regs.combo oper)), Nothing)
    Bxl ->
      (pointer.inc, regs.set B (regs.b `xor` oper.lit), Nothing)
    Bst ->
      (pointer.inc, regs.set B (regs.combo oper `mod` 8), Nothing)
    Bxc ->
      (pointer.inc, regs.set B (regs.b `xor` regs.c), Nothing)
    Out ->
      (pointer.inc, regs, Just $ regs.combo oper `mod` 8)

display (Cmd op oper) = case op of
  Jnz -> "JNZ"
  Div r ->
    show r ++ "= A div (2^" ++ show oper ++ ")"
  Bxl ->
    "B = B xor " ++ show oper.lit
  Bst ->
    "B = " ++ show oper ++ "% 8"
  Bxc ->
    "B = " ++ "B xor C"
  Out ->
    "print " ++ show oper

getReg A (a, b, c) = a
getReg B (a, b, c) = b
getReg C (a, b, c) = c

setReg x A (_, b, c) = (x, b, c)
setReg x B (a, _, c) = (a, x, c)
setReg x C (a, b, _) = (a, b, x)

solveA :: (Regs, [Cmd], _) -> IO ()
solveA (regs, cmds, _) = do
  let cmdMap = Map.fromList $ zip [0 ..] cmds
  let res = doAll cmdMap regs 0

  let output = res & mapMaybe (view _3)

  mprint cmdMap

  print "output"
  putStrLn $ intercalate "," $ map show output

--

-- B = (A % 8) xor 5
-- C = A div (2 ^ B)
-- B = B xor C xor 6
-- A = A div 8
-- print B
-- JNZ

-- B = 0
-- 0 = 0 xor C xor 6  => C = 6
-- C = A div 1 =>, A = 6
-- B =

-- B1 = (A % 8) xor 5
-- C = A div (2 ^ B1)
-- B2 = B1 xor C xor 6
-- A = A div 8
-- print B
-- JNZ

mod8 x = mod x 8

-- traceLab s x = if test then trace

-- readO

readOcto x = case readOct x of
  [(a, "")] -> a
  _ -> error x

debug = False

traceShow s x = if debug then Trace.traceShow s x else x

findDigit :: [Int] -> Int -> Int -> [[Int]]
findDigit [] cur p = [[]]
findDigit (target : ts) cur p = do
  q <- [0 .. 7]
  -- qq <- [1 .. 8]
  let a = (cur * 8) + q
  let b2 = traceShow ("cur", cur, "a", a) $ (a `mod` 8) `xor` 5
  let c = a `div` (2 ^ b2)
  let b3 = b2 `xor` c
  let b4 = b3 `xor` 6
  let b5 = b4 `mod` 8

  guard $ traceShow (cur, a, b5 == target) b5 == target

  map (a :) $ findDigit ts a (p + 1)

-- findDigit2 :: [Int] -> Int -> Int -> [[Int]]
-- findDigit2 [] cur p = [[cur]]
-- findDigit2 (target : ts) cur p = do

--   let b3 = target `xor` 6
--   let b2 = b3 `xor` c
--   let aMod = b2 `xor` 5
--   let a = (2 ^ b2) * cur

--   let res = a+aMod

--   -- guard $ b4 == target

--   map (res :) $ findDigit2 ts res 0

-- b2 = a 3 siste bitsa XOR 101
-- let b2 = (a `mod` 8) `xor` 5
-- c = fjerne 1-3 første bitsa a
-- let c = a `div` (2 ^ b2)
-- let b3 = b2 `xor` c
-- let b4 = b3 `xor` 6

-- B = ((A % 8) xor 6 xor (A div (2 ^ ((A % 8))))
-- A = A div 8
-- print B

solveB :: (Regs, [Cmd], [Int]) -> IO ()
solveB (startRegs, cmds, orgProg) = do
  let cmdMap = Map.fromList $ zip [0 ..] cmds

  for [1 .. 10000] $ \i -> do
    let testRegs = startRegs.set A i
    let res = doAll cmdMap testRegs 0
    let output = res & mapMaybe (view _3)
    when (output == [5, 5, 3, 0]) $ do
      print $ res & map (view _2 >>> (.a)) & nub & map (\i -> showOct i "")
      print $ res & map (view _2 >>> (.a)) & nub

  -- print "findDigit [0, 3] 0 0"
  mprint $ findDigit (reverse orgProg) 0 0 & map last & minimum

  -- print $ findDigit [3] "" 1

  -- print "findDigit"
  -- print $ findDigit [0, 3, 5] 0 0
  -- print "findDigit2"
  -- print $ findDigit [3] 6 1
  -- print "findDigit2"
  -- print $ findDigit 3 6 1

  -- for [0 ..] $ \i -> do
  --   let regs = startRegs.set A i
  --   let res = doAll cmdMap testRegs 0
  --   let output = res & mapMaybe (view _3)
  --   when (output == orgProg) $ do
  --     print i

  --   print i

  -- print $ map (\x -> xor 5 x) [0 :: Int .. 7]

  mprint $ Map.elems cmdMap
  mapM_ putStrLn $ map display $ Map.elems cmdMap

-- print "orgprog"
-- print orgProg

-- mprint res

testInput0 =
  [r|Register A: 3
Register B: 0
Register C: 0

Program: 2,4,1,5,7,5,4,3,1,6,0,3,5,5,3,0
|]

testInput1 =
  [r|Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
|]

testInput2 =
  [r|Register A: 0
Register B: 0
Register C: 9

Program: 2,6
|]
testInput3 =
  [r|Register A: 10
Register B: 0
Register C: 0

Program: 5,0,5,1,5,4
|]

testInput4 =
  [r|Register A: 0
Register B: 29
Register C: 0

Program: 1,7
|]

testInput5 =
  [r|Register A: 0
Register B: 2024
Register C: 43690

Program: 4,0
|]

testInput6 =
  [r|Register A: 2024
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
|]

testInput7 =
  [r|Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
|]

run :: String -> IO ()
run input = void $ do
  -- input <- putStrLn "#####    testInput   #####" >> pure testInput0
  -- print input
  let parsed = parse input
  -- mprint parsed
  let resA = solveA parsed
  resA

  let resB = solveB parsed
  resB

-- resA @=? 1715
-- let resB = solveB parsed
-- print resB
-- resB @=? 1739
