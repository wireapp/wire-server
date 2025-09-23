-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH

module Test.CargoHold.API.LogJSON
  ( withStructuredJSONLogger,
    lookupText,
    lookupBool,
  )
where

import qualified Control.Concurrent as CC
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified GHC.IO.Handle as IOH
import Imports
import qualified System.IO as IO
import qualified System.IO.Error as IOE
import System.Logger.Extended
import System.Posix.IO (closeFd, createPipe, fdToHandle)
import System.Posix.Types (Fd)
import UnliftIO.Exception (bracket, catch)

-- | Run an action with a logger configured to emit Structured JSON to stdout,
-- capturing stdout via a POSIX pipe into memory.
withStructuredJSONLogger :: forall a. (Logger -> IO a) -> IO (a, [Value])
withStructuredJSONLogger action = bracket acquire release use
  where
    acquire :: IO (Logger, IO.Handle, Fd, Fd, IO.Handle, Chan (Maybe Value), ThreadId)
    acquire = do
      (rfd, wfd) <- createPipe
      rH <- fdToHandle rfd
      wH <- fdToHandle wfd
      -- Keep a copy of stdout to restore later (not strictly needed now)
      oldStdout <- IOH.hDuplicate IO.stdout
      -- Configure logger to write directly to our pipe via /proc/self/fd/<wfd>
      let fdPath = "/proc/self/fd/" ++ show wfd
      logger <-
        new
          . setRenderer structuredJSONRenderer
          . setOutput (Path fdPath)
          . setLogLevel Info
          . setFormat Nothing
          $ defSettings
      ch <- newChan
      tid <- CC.forkIO $ do
        eAll <- IOE.tryIOError (BS8.hGetContents rH)
        case eAll of
          Left _ -> writeChan ch Nothing
          Right contents -> do
            let linesBS = BS8.split '\n' contents
            forM_ linesBS $ \ln ->
              case A.decode (LBS.fromStrict ln) of
                Just v -> writeChan ch (Just v)
                Nothing -> pure ()
            writeChan ch Nothing
      pure (logger, oldStdout, rfd, wfd, wH, ch, tid)

    release :: (Logger, IO.Handle, Fd, Fd, IO.Handle, Chan (Maybe Value), ThreadId) -> IO ()
    release (_logger, oldStdout, rfd, _wfd, _wH, _ch, tid) = do
      -- Restore stdout and close pipe fds
      IOH.hDuplicateTo oldStdout IO.stdout
      IO.hClose oldStdout
      closeFd rfd `catch` \(_ :: SomeException) -> pure ()
      CC.killThread tid `catch` \(_ :: SomeException) -> pure ()

    use :: (Logger, IO.Handle, Fd, Fd, IO.Handle, Chan (Maybe Value), ThreadId) -> IO (a, [Value])
    use (logger, _oldStdout, _rfd, _wfd, wH, ch, _tid) = do
      r <- action logger
      flush logger >> close logger
      -- Close the write handle to signal EOF
      IO.hClose wH
      let drain acc = do
            mv <- readChan ch
            case mv of
              Nothing -> pure (reverse acc)
              Just v -> drain (v : acc)
      logs <- drain []
      pure (r, logs)

lookupText :: Text -> Value -> Maybe Text
lookupText k (A.Object o) = case KM.lookup (Key.fromText k) o of
  Just (A.String t) -> Just t
  _ -> Nothing
lookupText _ _ = Nothing

lookupBool :: Text -> Value -> Maybe Bool
lookupBool k (A.Object o) = case KM.lookup (Key.fromText k) o of
  Just (A.Bool b) -> Just b
  _ -> Nothing
lookupBool _ _ = Nothing
