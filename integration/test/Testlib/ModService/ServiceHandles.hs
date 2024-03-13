module Testlib.ModService.ServiceHandles
  ( logHandleToConsole,
    logChanToConsole,
    flushChan,
    mkChans,
    LineOrEOF (..),
    ServiceHandle,
  )
where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad.Extra
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.String
import qualified GHC.IO.Exception as E (IOErrorType (EOF), ioe_type)
import System.IO
import System.Process
import Testlib.Printing
import Prelude

type ServiceHandle = (Chan LineOrEOF, Chan LineOrEOF, ProcessHandle)

data LineOrEOF = Line String | EOF
  deriving (Eq, Show)

logHandleToConsole :: String -> String -> Handle -> IO ()
logHandleToConsole execName domain hdl = do
  lns <- catchEOF (hGetLine hdl)
  logToConsoleWithEOF execName domain `mapM_` (pure <$> lns)

logChanToConsole :: String -> String -> Chan LineOrEOF -> IO ()
logChanToConsole execName domain chan =
  logToConsoleWithEOF execName domain (readChan chan)

-- | Shared implementation of 'log{Handle,Chan}ToConsole'.
logToConsoleWithEOF :: String -> String -> IO LineOrEOF -> IO ()
logToConsoleWithEOF execName domain feed = do
  let go =
        feed >>= \case
          Line line -> do
            putStrLn (decorateLine execName domain line)
            go
          EOF -> pure ()
  go

-- | Read everything from a channel and return it as a decorated multi-line String.
flushChan :: String -> String -> Chan LineOrEOF -> IO String
flushChan execName domain chan = do
  let go lns =
        readChan chan >>= \case
          Line ln -> go (ln : lns)
          EOF -> pure (reverse lns)
  (unlines . fmap (decorateLine execName domain)) <$> go []

-- | Run a thread that feeds output from a 'Handle' into two channels.
--
-- (We could also duplicate the posic handle, not the chan.  might save a few LOC.)
mkChans :: Handle -> IO (Chan LineOrEOF, Chan LineOrEOF)
mkChans hdl = do
  chn1 <- newChan
  chn2 <- dupChan chn1
  let go = do
        packet <- catchEOF (hGetLine hdl)
        writeList2Chan chn1 packet
        if EOF `elem` packet
          then pure ()
          else go
  void $ forkIO go
  pure (chn1, chn2)

-- | If 'SomeException' is thrown, show it, split up in lines, and feed it to the output
-- followed be '[EOF]'.  (But if the exception is 'EOF', do not add it to the output.)
catchEOF :: IO String -> IO [LineOrEOF]
catchEOF feed =
  (((: []) . Line) <$> feed)
    `E.catch` handleEOF
    `E.catch` handleEverythingElse
  where
    handleEOF :: E.IOException -> IO [LineOrEOF]
    handleEOF e =
      if E.ioe_type e == E.EOF
        then pure [EOF]
        else renderErr e

    handleEverythingElse :: E.SomeException -> IO [LineOrEOF]
    handleEverythingElse e = renderErr e

    renderErr :: E.Exception e => e -> IO [LineOrEOF]
    renderErr e = pure $ (Line <$> lines (show e)) <> [EOF]

decorateLine :: String -> String -> String -> String
decorateLine execName domain = colorize . (prefix <>)
  where
    prefix = "[" <> execName <> "@" <> domain <> "] "
    colorize = fromMaybe id (lookup execName processColors)

processColors :: [(String, String -> String)]
processColors =
  [ ("brig", colored green),
    ("galley", colored yellow),
    ("gundeck", colored blue),
    ("cannon", colored orange),
    ("cargohold", colored purpleish),
    ("spar", colored orange),
    ("federator", colored blue),
    ("background-worker", colored blue),
    ("nginx", colored purpleish)
  ]
