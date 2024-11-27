module Day.Day01 (run) where

parse = id

run :: String -> IO ()
run input = do
  let parsed = parse input
  print parsed
