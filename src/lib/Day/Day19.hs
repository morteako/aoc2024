module Day.Day19 (run) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad (void)
import Control.Monad.State (
  State,
  evalState,
  get,
  modify,
 )
import Data.ByteString qualified as BS
import Data.List.Extra (splitOn)
import Data.String (fromString)
import Data.Traversable (for)
import Data.Trie (Trie)
import Data.Trie qualified as Trie

import Control.Lens (foldlMOf)
import Control.Monad.Extra (fold1M)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Test.HUnit ((@=?))
import Utils (apply2, countP)

parse :: String -> ([ByteString], [ByteString])
parse = lines >>> splitOn [""] >>> apply2 (,) >>> over _1 (head >>> splitOn ", ") >>> over each (map fromString)

countCombs :: Trie a -> ByteString -> State (HashMap ByteString Int) Int
countCombs trie bs = do
  cache <- get
  case HashMap.lookup bs cache of
    Just cached -> pure cached
    Nothing -> do
      res <- fmap sum $ for (Trie.matches trie bs) $ \(_, _, rem) -> countCombs trie rem
      modify (HashMap.insert bs res)
      pure res

run :: String -> IO ()
run input = void $ do
  let (patterns, designs) = parse input

  let trie = Trie.fromList $ map (\bs -> (bs, BS.length bs)) patterns

  let combCounts = evalState (traverse (countCombs trie) designs) (HashMap.singleton "" 1)

  let resA = countP (> 0) combCounts
  print resA
  resA @=? 342

  let resB = sum combCounts
  print resB
  resB @=? 891192814474630
