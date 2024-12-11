module Main (
  main,
) where

import CmdArgs
import Control.Monad (join, void)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Map qualified as Map
import Data.Map.Strict qualified as Map
import DayVersion (DayVersion, getDayNum)
import Input (getInput)
import Options.Applicative (execParser)
import Solutions (solutions)
import TimeIt (timeItNamed)
import Utils ((=:))

runner :: Options -> IO ()
runner o@Options{day, input} = do
  let lastDayNr :: DayVersion
      lastDayRunnner :: String -> IO ()
      (lastDayNr, lastDayRunnner) = Map.findMax solutions
  print lastDayNr
  let func :: String -> IO ()
      func i = case day of
        LastDay ->
          lastDayRunnner i
        SpecificDay d ->
          case Map.lookup d solutions of
            Nothing -> do
              putStrLn $ show d <> " is not implemented."
              putStrLn $ "Currently implemented : " <> unwords (show <$> Map.keys solutions)
            Just dayRunner ->
              dayRunner i
  timeItNamed "WithInput" $ do
    inputFile <- case input of
      StdIn -> do
        getContents
      File path -> do
        readFile path
      Test -> do
        let path = "inputs/" <> "/" <> show lastDayNr <> "test"
        readFile path
      DayInput -> do
        case day of
          LastDay -> getInput $ getDayNum lastDayNr
          SpecificDay d -> getInput $ getDayNum d
    putStr "> "
    print o
    timeItNamed "Runner" $ func inputFile

main :: IO ()
main = do
  let parser = cmdParser
  execParser parser >>= void . runner
