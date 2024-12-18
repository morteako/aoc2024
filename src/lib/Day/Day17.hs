{-# LANGUAGE StrictData #-}

module Day.Day17 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (guard, void, when)
import Data.Bits (Bits (xor))
import Data.IntMap qualified as Map
import Data.List (intercalate, isInfixOf, sort)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (mapMaybe)
import GHC.Records
import Test.HUnit ((@=?))
import Utils (readInt)

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
        then (succ pointer, regs, Nothing)
        else (oper.lit, regs, Nothing)
    Div r ->
      (succ pointer, regs.set r (regs.r A `div` (2 ^ regs.combo oper)), Nothing)
    Bxl ->
      (succ pointer, regs.set B (regs.b `xor` oper.lit), Nothing)
    Bst ->
      (succ pointer, regs.set B (regs.combo oper `mod` 8), Nothing)
    Bxc ->
      (succ pointer, regs.set B (regs.b `xor` regs.c), Nothing)
    Out ->
      (succ pointer, regs, Just $ regs.combo oper `mod` 8)

getReg A (a, b, c) = a
getReg B (a, b, c) = b
getReg C (a, b, c) = c

setReg x A (_, b, c) = (x, b, c)
setReg x B (a, _, c) = (a, x, c)
setReg x C (a, b, _) = (a, b, x)

solveA :: (Regs, [Cmd], _) -> String
solveA (regs, cmds, _) = do
  let cmdMap = Map.fromList $ zip [0 ..] cmds
  let res = doAll cmdMap regs 0

  let output = res & mapMaybe (view _3)

  intercalate "," $ map show output

-- Hard coded for input :))
findDigit :: [Int] -> Int -> Int -> [[Int]]
findDigit [] cur p = [[]]
findDigit (target : ts) cur p = do
  q <- [0 .. 7]
  let a = (cur * 8) + q
  let b2 = (a `mod` 8) `xor` 5
  let c = a `div` (2 ^ b2)
  let b3 = b2 `xor` c
  let b4 = b3 `xor` 6
  let b5 = b4 `mod` 8

  guard $ b5 == target

  map (a :) $ findDigit ts a (p + 1)

solveB :: (Regs, [Cmd], [Int]) -> Int
solveB (startRegs, cmds, orgProg) = do
  let cmdMap = Map.fromList $ zip [0 ..] cmds
  findDigit (reverse orgProg) 0 0 & map last & minimum

run :: String -> IO ()
run input = void $ do
  let parsed = parse input

  let resA = solveA parsed
  print resA
  resA @=? "7,3,5,7,5,7,4,3,0"

  let resB = solveB parsed
  print resB
  resB @=? 105734774294938
