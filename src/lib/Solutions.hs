module Solutions where

import Data.Map (Map)
import Data.Map qualified as Map
import Day.Day01 qualified
import Day.Day02 qualified
import Day.Day03 qualified

import DayVersion
import Utils ((=:))

solutions :: Map DayVersion (String -> IO ())
solutions =
  Map.fromList
    [ "1" =: Day.Day01.run
    , "2" =: Day.Day02.run
    , "3" =: Day.Day03.run
    -- , "4" =: Day.Day04.run
    -- , "5" =: Day.Day05.run
    -- , "6" =: Day.Day06.run
    -- , "7" =: Day.Day07.run
    -- , "8" =: Day.Day08.run
    -- , "9" =: Day.Day09.run
    -- , "10" =: Day.Day10.run
    -- , "11" =: Day.Day11.run
    -- , "12" =: Day.Day12.run
    -- , "13" =: Day.Day13.run
    -- , "14" =: Day.Day14.run
    -- , "15" =: Day.Day15.run
    -- , "16" =: Day.Day16.run
    -- , "17" =: Day.Day17.run
    -- , "18" =: Day.Day18.run
    -- , "20" =: Day.Day20.run
    -- , "21" =: Day.Day21.run
    -- , "22" =: Day.Day22.run
    -- , "25" =: Day.Day25.run
    ]
