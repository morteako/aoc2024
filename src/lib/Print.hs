{-# LANGUAGE UndecidableInstances #-}

module Print where

import Data.Map as Map

printlab :: (Show a) => [Char] -> a -> IO ()
printlab s a = putStr (s ++ " ") >> print a

class Print a where
    mprint :: a -> IO ()

instance Print String where
    mprint = print

instance {-# INCOHERENT #-} (Show a) => Print [a] where
    mprint = mapM_ print

instance {-# INCOHERENT #-} (Show a) => Print a where
    mprint = print

instance {-# INCOHERENT #-} (Show k, Show v) => Print (Map k v) where
    mprint = mapM_ (putStrLn . (\(k, v) -> show k ++ " => " ++ show v)) . Map.toList