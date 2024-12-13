module Day.Day13 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (void)
import Data.List (sort)
import Data.List.Extra (splitOn, sumOn')
import Data.Maybe (catMaybes)
import Test.HUnit ((@=?))

data Group = Group (Integer, Integer) (Integer, Integer) (Integer, Integer) deriving (Show)

parse :: [Char] -> [Group]
parse = filter (/= ',') >>> lines >>> fmap words >>> splitOn [[]] >>> fmap parseGroup
 where
  parseGroup [a, b, target] = Group (parseButton a) (parseButton b) (parseTarget target)

  parseButton [_, _, 'X' : '+' : x, 'Y' : '+' : y] = (read x, read y)
  parseTarget [_, 'X' : '=' : xtarget, 'Y' : '=' : ytarget] = (read xtarget, read ytarget)

getSolution :: Group -> Maybe (Integer, Integer)
getSolution g@(Group (x, y) (m, n) (r, t))
  | divvy == 0 = Nothing
  | checkA && checkB =
      Just (a, b)
  | otherwise = Nothing
 where
  checkA = a * x + b * m == r
  checkB = a * y + b * n == t
  a = (r - b * m) `div` x
  ab0 = r `div` x
  divvy = (n * x - y * m)
  b = ((t * x) - (r * y)) `div` divvy

-- Can be skipped since the input does include cases like
-- x+1 y+1
-- x+3 y+3
-- x=3 y=3
-- getBothCorrect :: Group -> Maybe Integer
-- getBothCorrect g@(Group (x, y) (m, n) (r, t)) =
--   [ f g
--   , getQR r x m
--   , swap <$> getQR t y m
--   ]
--     & catMaybes
--     & filter
--       ( \(a, b) ->
--           let
--             checkA = a * x + b * m == r
--             checkB = a * y + b * n == t
--            in
--             checkA && checkB
--       )
--     & fmap calcVal
--     & sort
--     & listToMaybe
--       where
--         getQR target a b =
--           let
--             (q, r) = quotRem target a
--             (qq, rr) = quotRem r b
--           in
--             if
--               | r == 0 -> Just (q, 0)
--               | rr == 0 -> Just (q, qq)
--               | otherwise -> Nothing

calcValue :: (Integer, Integer) -> Integer
calcValue = (\(a, b) -> a * 3 + b)

run :: String -> IO ()
run input = void $ do
  let groups = parse $ input

  let resA = fmap getSolution groups & catMaybes & sumOn' calcValue
  print resA
  resA @=? 28262

  let newGroups = fmap (\(Group a b (ta, tb)) -> Group a b (ta + 10000000000000, tb + 10000000000000)) groups
  let resB = fmap getSolution newGroups & catMaybes & sumOn' calcValue
  print resB
  resB @=? 101406661266314
