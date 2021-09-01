-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Federator.Monitor.Internal where

import Control.Exception (try)
import Data.ByteString (packCStringLen)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.X509 as X509
import Data.X509.CertificateStore
import Federator.Env (TLSSettings (..))
import Federator.Options (RunSettings (..))
import GHC.Foreign (withCStringLen)
import GHC.IO.Encoding (getFileSystemEncoding)
import Imports
import qualified Network.TLS as TLS
import Polysemy (Embed, Member, Members, Sem, embed)
import qualified Polysemy
import qualified Polysemy.Error as Polysemy
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import System.FilePath (splitFileName)
import System.INotify
import System.Logger (Logger)
import qualified System.Logger.Message as Log
import System.Posix.ByteString (RawFilePath)
import System.X509

-- This is needed because the normal Posix file system API uses strings, while
-- the inotify API uses bytestrings.
-- /Note/: File paths are strings obtained using the "file system encoding",
-- which is the same as the locale encoding, but uses some escaping tricks to
-- be able to represent arbitrary data as strings.
rawPath :: FilePath -> IO RawFilePath
rawPath path = do
  encoding <- getFileSystemEncoding
  withCStringLen encoding path packCStringLen

data WatchedPath
  = WatchedFile RawFilePath
  | WatchedDir RawFilePath (Set RawFilePath)
  deriving (Eq, Ord, Show)

mergePaths :: [WatchedPath] -> (Set WatchedPath)
mergePaths = Set.fromList . merge . sort
  where
    merge [] = []
    merge [w] = [w]
    merge (w1 : w2 : ws) = case (w1, w2) of
      (_, WatchedFile _) -> w1 : w2 : merge ws
      (WatchedDir dir1 paths1, WatchedDir dir2 paths2)
        | dir1 == dir2 -> merge (WatchedDir dir1 (paths1 <> paths2) : ws)
      _ -> w1 : merge (w2 : ws)

watchedPath :: WatchedPath -> RawFilePath
watchedPath (WatchedFile path) = path
watchedPath (WatchedDir dir _) = dir

-- Since we are watching a filesystem path, and not an inode, we need to replace a
-- file watch when the file gets overwritten.
-- This type is a map of paths to watches used to keep track of both file and
-- directory watches as they get deleted and recreated.
type Watches = Map RawFilePath WatchDescriptor

runMonitor :: Logger -> Sem '[TinyLog, Embed IO] a -> IO a
runMonitor logger = Polysemy.runM . Log.runTinyLog logger

logErrors ::
  Members '[TinyLog, Polysemy.Error FederationSetupError] r =>
  Sem r a ->
  Sem r a
logErrors action = Polysemy.catch action $ \err -> do
  Log.err $
    Log.msg ("federation setup error while updating certificates" :: Text)
      . Log.field "error" (showFederationSetupError err)
  Polysemy.throw err

logAndIgnoreErrors ::
  Member TinyLog r =>
  Sem (Polysemy.Error FederationSetupError ': r) () ->
  Sem r ()
logAndIgnoreErrors = void . Polysemy.runError . logErrors

stopMonitoringCertificates ::
  (Members '[TinyLog, Embed IO] r) =>
  IORef Watches ->
  Sem r ()
stopMonitoringCertificates watchesVar = do
  watches <- readIORef watchesVar
  traverse_ stop watches
  where
    stop wd = do
      -- ignore exceptions when removing watches
      embed . void . try @IOException $ removeWatch wd
      Log.debug $
        Log.msg ("stopped watching file" :: Text)
          . Log.field "descriptor" (show wd)

monitorCertificates ::
  ( Members '[TinyLog, Embed IO] r,
    Members '[TinyLog, Embed IO, Polysemy.Error FederationSetupError] r1
  ) =>
  (Sem r1 () -> IO ()) ->
  IORef TLSSettings ->
  RunSettings ->
  Sem r (IORef Watches)
monitorCertificates runSem tlsVar rs = do
  inotify <- embed initINotify
  watchesVar <- embed @IO $ newIORef mempty
  let watch wpath = do
        let handler = handleEvent inotify runSem wpath tlsVar watchesVar rs
        case wpath of
          WatchedFile path -> do
            wd <- embed $ addFileWatch inotify watchesVar path handler
            Log.debug $
              Log.msg ("watching file" :: Text)
                . Log.field "descriptor" (show wd)
                . Log.field "file" path
          WatchedDir path _ -> do
            wd <- embed $ addDirectoryWatch inotify watchesVar path handler
            Log.debug $
              Log.msg ("watching directory" :: Text)
                . Log.field "descriptor" (show wd)
                . Log.field "path" path
  Log.debug $
    Log.msg ("inotify initialized" :: Text)
      . Log.field "inotify" (show inotify)
  paths <- embed $ certificateWatchPaths rs
  traverse_ watch (toList paths)
  pure watchesVar

data Action = ReplaceWatch RawFilePath | ReloadSettings
  deriving (Eq, Ord, Show)

handleEvent ::
  Members '[TinyLog, Embed IO, Polysemy.Error FederationSetupError] r =>
  INotify ->
  (Sem r () -> IO ()) ->
  WatchedPath ->
  IORef TLSSettings ->
  IORef Watches ->
  RunSettings ->
  Event ->
  IO ()
handleEvent inotify runSem wpath tlsVar watchesVar rs e = do
  let actions = getActions wpath e
  unless (null actions) $
    -- only use runSem when there are some actions
    -- this makes it possible to use a special runSem in the tests that is able
    -- to detect when some action has taken place
    runSem $ traverse_ (applyAction inotify runSem tlsVar watchesVar rs) actions

-- Note: it is important that the watch is replaced *before* settings are
-- reloaded, otherwise there is a window of time (after reloading settings,
-- but before the new watch is set) where changes to the settings can go
-- undetected
getActions :: WatchedPath -> Event -> [Action]
getActions (WatchedFile path) (Closed _ mpath True)
  | maybe True (== path) mpath = [ReloadSettings]
getActions (WatchedDir dir paths) (MovedIn _ path _)
  | Set.member path paths = [ReplaceWatch (dir <> path), ReloadSettings]
getActions (WatchedDir dir paths) (Created _ path)
  | Set.member path paths = [ReplaceWatch (dir <> path), ReloadSettings]
getActions _ _ = []

applyAction ::
  (Members '[TinyLog, Embed IO, Polysemy.Error FederationSetupError] r) =>
  INotify ->
  (Sem r () -> IO ()) ->
  IORef TLSSettings ->
  IORef Watches ->
  RunSettings ->
  Action ->
  Sem r ()
applyAction _ _ tlsVar _ rs ReloadSettings = do
  tls' <- mkTLSSettings rs
  Log.info $ Log.msg ("updating TLS settings" :: Text)
  embed @IO $ atomicWriteIORef tlsVar tls'
applyAction inotify runSem tlsVar watchesVar rs (ReplaceWatch path) = do
  let pathText = Text.decodeUtf8With Text.lenientDecode path
  Log.debug $
    Log.msg ("replacing watch" :: Text)
      . Log.field "path" pathText
  r <-
    embed @IO . try @SomeException $
      addFileWatch
        inotify
        watchesVar
        path
        (handleEvent inotify runSem (WatchedFile path) tlsVar watchesVar rs)
  case r of
    Right _ -> pure ()
    Left e -> do
      Log.err $
        Log.msg ("error while replacing watch" :: Text)
          . Log.field "path" pathText
          . Log.field "error" (displayException e)

addDirectoryWatch ::
  INotify ->
  IORef Watches ->
  RawFilePath ->
  (Event -> IO ()) ->
  IO WatchDescriptor
addDirectoryWatch inotify = addWatchAndSave inotify [MoveIn, Create]

addFileWatch ::
  INotify ->
  IORef Watches ->
  RawFilePath ->
  (Event -> IO ()) ->
  IO WatchDescriptor
addFileWatch inotify = addWatchAndSave inotify [CloseWrite]

addWatchAndSave ::
  INotify ->
  [EventVariety] ->
  IORef Watches ->
  RawFilePath ->
  (Event -> IO ()) ->
  IO WatchDescriptor
addWatchAndSave inotify events watchesVar path handler = do
  -- create a new watch
  w' <- addWatch inotify events path handler
  -- atomically save it in the map, and return the old one
  mw <-
    atomicModifyIORef watchesVar $
      swap . Map.alterF (,Just w') path
  -- remove the old watch
  case mw of
    Nothing -> pure ()
    Just w -> removeWatch w
  pure w'

certificatePaths :: RunSettings -> [FilePath]
certificatePaths rs =
  maybeToList (remoteCAStore rs)
    ++ [ clientCertificate rs,
         clientPrivateKey rs
       ]

certificateWatchPaths :: RunSettings -> IO (Set WatchedPath)
certificateWatchPaths =
  fmap (mergePaths . concat)
    . traverse watched
    . certificatePaths
  where
    watched :: FilePath -> IO [WatchedPath]
    watched path = do
      rpath <- rawPath path
      let (dir, base) = splitFileName path
      rdir <- rawPath dir
      rbase <- rawPath base
      pure
        [ WatchedFile rpath,
          WatchedDir rdir (Set.singleton rbase)
        ]

data FederationSetupError
  = InvalidCAStore FilePath
  | InvalidClientCertificate String
  deriving (Show)

instance Exception FederationSetupError

showFederationSetupError :: FederationSetupError -> Text
showFederationSetupError (InvalidCAStore path) = "invalid CA store: " <> Text.pack path
showFederationSetupError (InvalidClientCertificate msg) = Text.pack msg

mkTLSSettings ::
  Members '[Embed IO, Polysemy.Error FederationSetupError] r =>
  RunSettings ->
  Sem r TLSSettings
mkTLSSettings settings =
  TLSSettings
    <$> mkCAStore settings
    <*> mkCreds settings

mkCAStore ::
  Members '[Embed IO, Polysemy.Error FederationSetupError] r =>
  RunSettings ->
  Sem r CertificateStore
mkCAStore settings = do
  customCAStore <- fmap (fromRight mempty) . Polysemy.runError @() $ do
    path <- maybe (Polysemy.throw ()) pure $ remoteCAStore settings
    embed (readCertificateStore path)
      >>= maybe (Polysemy.throw (InvalidCAStore path)) pure
  systemCAStore <-
    if useSystemCAStore settings
      then embed getSystemCertificateStore
      else pure mempty
  pure (customCAStore <> systemCAStore)

mkCreds ::
  forall r.
  Members '[Embed IO, Polysemy.Error FederationSetupError] r =>
  RunSettings ->
  Sem r TLS.Credential
mkCreds settings = do
  creds <-
    Polysemy.fromExceptionVia
      @SomeException
      (InvalidClientCertificate . displayException)
      $ TLS.credentialLoadX509
        (clientCertificate settings)
        (clientPrivateKey settings)
  case creds of
    Left e -> Polysemy.throw (InvalidClientCertificate e)
    Right (X509.CertificateChain [], _) ->
      Polysemy.throw
        ( InvalidClientCertificate
            "could not read client certificate"
        )
    Right x -> pure x
