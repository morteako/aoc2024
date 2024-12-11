{-# LANGUAGE TemplateHaskell #-}

module Day.Day09 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (void)
import Control.Monad.Extra (unit)
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Foldable.Extra (sumOn')
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (foldl', mapAccumL)
import Data.List.Split (chunksOf)
import Test.HUnit ((@=?))
import Utils (charRead)
import Prelude hiding (id)

data Group = GPlace Place | Space {scount :: Int}
data Place = Place {id :: Int, pcount :: Int}

parse :: [Char] -> [Group]
parse = fmap charRead >>> chunksOf 2 >>> imap f >>> concat
 where
  f i [c, 0] = GPlace (Place i c) : []
  f i [c, freeSpace] = GPlace (Place i c) : Space freeSpace : []
  f i [c] = GPlace (Place i c) : []
  f i xs = error $ show xs

getValidSpace :: IntMap Int -> (Int, Place) -> Maybe (Int, Int)
getValidSpace spaces (placeIndex, place) =
  IntMap.foldrWithKey findFirstValidSpace Nothing spaces
 where
  findFirstValidSpace ind spaceCount rest
    | ind >= placeIndex = Nothing
    | spaceCount >= place.pcount = Just (ind, spaceCount)
    | otherwise = rest

data S = S {_movedSpaces :: IntMap Int, _places :: IntMap [Place], _spaces :: IntMap Int}
makeLenses ''S

findOne :: (Int, Place) -> State S ()
findOne (placeIndex, place) = do
  s <- get
  case getValidSpace s._spaces (placeIndex, place) of
    Nothing -> pure ()
    Just (spaceInd, spaceCount) -> do
      places %= (IntMap.delete placeIndex >>> IntMap.insertWith (++) spaceInd [place])
      spaces
        %= ( if spaceCount > place.pcount
              then IntMap.insert spaceInd (spaceCount - place.pcount)
              else IntMap.delete spaceInd
           )
      movedSpaces %= IntMap.insert placeIndex place.pcount

start :: [Group] -> Int
start groups = do
  let (places, spaces) = zip [0 ..] groups & IntMap.fromList & IntMap.mapEither toEither
       where
        toEither = \case
          GPlace p -> Left p
          Space s -> Right s

  let startState = S{_movedSpaces = IntMap.empty, _places = fmap pure places, _spaces = spaces}

  let resState =
        traverse_ findOne (IntMap.toDescList places)
          & flip execState startState

  let combSpaces = (resState._movedSpaces <> resState._spaces) & fmap (pure . Space)

  let orderedGroups = IntMap.unionWith (++) combSpaces (fmap (fmap GPlace) resState._places) & fmap reverse & concat

  let (_, scores) = mapAccumL (\cur space -> (calcScore cur space)) 0 orderedGroups
  sum scores

calcScore :: Int -> Group -> (Int, Int)
calcScore index (Space count) = (index + count, 0)
calcScore index (GPlace p) = (index + p.pcount, sum $ [p.id * (index + i) | i <- [0 .. p.pcount - 1]])

flattenGroup :: Group -> [Group]
flattenGroup (Space count) = replicate count (Space 1)
flattenGroup (GPlace p) = replicate p.pcount (GPlace (Place{id = p.id, pcount = 1}))

run :: String -> IO ()
run input = void $ do
  let parsed = parse input

  let resA = start $ concatMap flattenGroup parsed
  print resA
  resA @=? 6415184586041

  let resB = start parsed
  print resB
  resB @=? 6436819084274
