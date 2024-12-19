module Trace where

import Data.IORef (newIORef, readIORef, writeIORef)
import Debug.Trace qualified as T
import GHC.IO (unsafePerformIO)
import GHC.IO.Unsafe (unsafeDupablePerformIO)
import GHC.IORef (IORef)

{-# NOINLINE trace #-}
trace :: String -> a -> a
trace s a = if unsafeDupablePerformIO (readIORef debug) then T.trace (yellow s) a else a

traceLab, tl :: (Show a) => String -> a -> a
traceLab s x = T.trace (green s ++ ": " ++ yellow (show x)) x
tl = traceLab

traceOn, to :: (Show a) => (a -> String) -> a -> a
traceOn f x = trace (f x) x
to = traceOn

traceShow, ts :: (Show a) => a -> b -> b
traceShow s = trace (show s)
ts = traceShow

traceShowId, tsi :: (Show a) => a -> a
traceShowId a = trace (show a) a
tsi = traceShowId

yellow :: String -> String
yellow s = "\ESC[93m" ++ s ++ "\ESC[0m"

green :: String -> String
green s = "\ESC[92m" ++ s ++ "\ESC[0m"

isDebug :: Bool
isDebug = unsafeDupablePerformIO $ readIORef debug

debug :: IORef Bool
debug = unsafeDupablePerformIO $ newIORef True

setDebug :: Bool -> IO ()
setDebug b = writeIORef debug b
