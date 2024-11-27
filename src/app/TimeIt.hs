module TimeIt (timeIt, timeItShow, timeItNamed, timeItT) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import System.CPUTime
import Text.Printf

-- | Wrap a 'MonadIO' computation so that it prints out the execution time.
timeIt :: (MonadIO m) => m a -> m a
timeIt = timeItNamed "CPU time"

{- | Like 'timeIt', but uses the 'show' rendering of @a@ as label for the
timing.

@since 2.0
-}
timeItShow :: (MonadIO m, Show a) => m a -> m a
timeItShow ioa = do
    (t, a) <- timeItT ioa
    liftIO $ printf (show a ++ ": %6.2fms\n") t
    return a

{- | Like 'timeIt', but uses the 'String' as label for the timing.

@since 2.0
-}
timeItNamed :: (MonadIO m) => String -> m a -> m a
timeItNamed name ioa = do
    (t, a) <- timeItT ioa
    liftIO $ printf (name ++ ": %6.2fms\n") t
    return a

{- | Wrap a 'MonadIO' computation so that it returns execution time in millis,
as well as the result value.
-}
timeItT :: (MonadIO m) => m a -> m (Double, a)
timeItT ioa = do
    t1 <- liftIO getCPUTime
    a <- ioa
    t2 <- liftIO getCPUTime
    let t :: Double
        t = fromIntegral (t2 - t1) * 1e-9
    return (t, a)