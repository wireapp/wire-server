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
import qualified Data.Set as Set
import qualified Data.Text as Text
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

monitorEvents :: [EventVariety]
monitorEvents = [CloseWrite, MoveIn, Create]

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
  [WatchDescriptor] ->
  Sem r ()
stopMonitoringCertificates = traverse_ stop
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
  ( Sem r1 () ->
    IO ()
  ) ->
  IORef TLSSettings ->
  RunSettings ->
  Sem r [WatchDescriptor]
monitorCertificates runSem tlsVar rs = do
  inotify <- embed initINotify
  let watch path = do
        wd <-
          embed
            . addWatch inotify monitorEvents (watchedPath path)
            . handleEvent runSem path tlsVar
            $ rs
        Log.debug $
          Log.msg ("watching file" :: Text)
            . Log.field "descriptor" (show wd)
            . Log.field "file" (watchedPath path)
        pure wd
  Log.debug $
    Log.msg ("inotify initialized" :: Text)
      . Log.field "inotify" (show inotify)
  paths <- embed $ certificateWatchPaths rs
  traverse watch (toList paths)

handleEvent ::
  Members '[TinyLog, Embed IO, Polysemy.Error FederationSetupError] r =>
  (Sem r () -> IO ()) ->
  WatchedPath ->
  IORef TLSSettings ->
  RunSettings ->
  Event ->
  IO ()
handleEvent runSem wpath var rs e = do
  when (needReload wpath e) $ do
    runSem (reloadSettings var rs)
  where
    needReload :: WatchedPath -> Event -> Bool
    needReload (WatchedFile path) (Closed _ mpath True) =
      maybe True (== path) mpath
    needReload (WatchedDir _ paths) (Closed _ (Just path) True) =
      Set.member path paths
    needReload (WatchedDir _ paths) (MovedIn _ path _) =
      Set.member path paths
    needReload (WatchedDir _ paths) (Created _ path) =
      Set.member path paths
    needReload _ _ = False

reloadSettings ::
  (Members '[TinyLog, Embed IO, Polysemy.Error FederationSetupError] r) =>
  IORef TLSSettings ->
  RunSettings ->
  Sem r ()
reloadSettings var rs = do
  tls' <- mkTLSSettings rs
  Log.info $ Log.msg ("updating TLS settings" :: Text)
  embed @IO $ atomicWriteIORef var tls'

certificatePaths :: RunSettings -> [FilePath]
certificatePaths rs =
  maybeToList (remoteCAStore rs)
    ++ [ clientCertificate rs,
         clientPrivateKey rs
       ]

-- TODO: consider only watching directories
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
            "could not read client certificate or private key"
        )
    Right x -> pure x
