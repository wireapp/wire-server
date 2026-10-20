-- | Helpers shared by "FanInPerf.Produce" and "FanInPerf.Consume" that do not
-- care whether the workers write or read.
module FanInPerf.Worker
  ( newErrorReporter,
    printQueuedErrors,
    waitForStop,
    isInterrupt,
  )
where

import Control.Exception (AsyncException (UserInterrupt), handleJust)
import Data.Misc (showT)
import FanInPerf.Terminal
import Imports

-- | A bounded queue of error messages plus the function that fills it. Error
-- path only; drops messages when the ticker falls behind.
newErrorReporter :: IO (TBQueue Text, Text -> IO ())
newErrorReporter = do
  errors <- newTBQueueIO 1000
  let reportError msg = atomically $ do
        full <- isFullTBQueue errors
        unless full (writeTBQueue errors msg)
  pure (errors, reportError)

-- | Prints queued error messages (at most 'maxErrorLines' per call).
printQueuedErrors :: Console -> TBQueue Text -> IO ()
printQueuedErrors console errors = do
  msgs <- atomically (flushTBQueue errors)
  traverse_ (printLine console) (take maxErrorLines msgs)
  when (length msgs > maxErrorLines) $
    printLine console ("... " <> showT (length msgs - maxErrorLines) <> " more errors suppressed")

maxErrorLines :: Int
maxErrorLines = 10

-- | Returns after @duration@ seconds or on Ctrl-C (GHC delivers SIGINT as
-- 'UserInterrupt' to the main thread).
waitForStop :: Maybe Int -> IO ()
waitForStop mDuration = handleJust isInterrupt pure sleepFor
  where
    sleepFor = maybe (forever (threadDelay 1_000_000)) (\d -> threadDelay (d * 1_000_000)) mDuration

isInterrupt :: AsyncException -> Maybe ()
isInterrupt UserInterrupt = Just ()
isInterrupt _ = Nothing
