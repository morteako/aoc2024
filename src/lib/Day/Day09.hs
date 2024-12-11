module Day.Day09 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (void)
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

findOne :: IntMap Int -> IntMap [Place] -> IntMap Int -> (Int, Place) -> (IntMap Int, IntMap [Place], IntMap Int)
findOne movedSpaces res spaces (placeIndex, place) =
  case getValidSpace spaces (placeIndex, place) of
    Nothing -> (movedSpaces, res, spaces)
    Just (spaceInd, spaceCount) ->
      let
        updatedRes =
          res
            & IntMap.delete placeIndex
            & IntMap.insertWith (++) spaceInd [place]

        updatedSpaces =
          if spaceCount > place.pcount
            then IntMap.insert spaceInd (spaceCount - place.pcount) spaces
            else IntMap.delete spaceInd spaces

        updatedMovesSpaces =
          IntMap.insert placeIndex place.pcount movedSpaces
       in
        (updatedMovesSpaces, updatedRes, updatedSpaces)

start :: [Group] -> Int
start groups = do
  let indexedGroupMap = zip [0 ..] groups & IntMap.fromList

  let toEither = \case
        GPlace p -> Left p
        Space s -> Right s

  let (places, spaces) = indexedGroupMap & IntMap.mapEither toEither

  let revPlaces = IntMap.toDescList places

  let listPlaces = fmap (: []) places

  let (movedSpaces, resPlaces, resSpaces) =
        foldl' (\(ms, r, s) ip -> findOne ms r s ip) (IntMap.empty, listPlaces, spaces) revPlaces
          & over _2 (fmap (fmap GPlace))

  let combSpaces = (movedSpaces <> resSpaces) & fmap ((: []) . Space)

  let orderedGroups = IntMap.unionWith (++) combSpaces resPlaces & fmap reverse & concat

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
