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

module Federator.Monitor
  ( withMonitor,
    mkTLSSettings,
    FederationSetupError (..),
  )
where

import Control.Exception (bracket, handle, throw)
import Control.Lens (view)
import Data.ByteString (packCStringLen)
import qualified Data.Set as Set
import qualified Data.X509 as X509
import Data.X509.CertificateStore
import Federator.Env
import Federator.Options (RunSettings (..))
import GHC.Foreign (withCStringLen)
import GHC.IO.Encoding (getFileSystemEncoding)
import Imports
import qualified Network.TLS as TLS
import Polysemy (Embed, Members, Sem, embed)
import qualified Polysemy
import qualified Polysemy.Error as Polysemy
import Polysemy.TinyLog (TinyLog)
import qualified Polysemy.TinyLog as Log
import System.FilePath (takeDirectory)
import System.INotify
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

monitorEvents :: [EventVariety]
monitorEvents = [CloseWrite, MoveIn, Create]

runMonitor :: Env -> Sem '[TinyLog, Embed IO] a -> IO a
runMonitor env = Polysemy.runM . Log.runTinyLog (view applog env)

withMonitor :: Env -> RunSettings -> IO a -> IO a
withMonitor env rs action =
  bracket
    (runMonitor env (monitorCertificates env rs))
    (runMonitor env . stopMonitoringCertificates)
    (const action)

stopMonitoringCertificates ::
  (Members '[TinyLog, Embed IO] r) =>
  [WatchDescriptor] ->
  Sem r ()
stopMonitoringCertificates = traverse_ stop
  where
    stop wd = do
      embed $ removeWatch wd
      Log.debug $
        Log.msg ("stopped watching file" :: Text)
          . Log.field "descriptor" (show wd)

monitorCertificates ::
  (Members '[TinyLog, Embed IO] r) =>
  Env ->
  RunSettings ->
  Sem r [WatchDescriptor]
monitorCertificates env rs = do
  inotify <- embed initINotify
  let watch path = do
        wd <- embed
          . addWatch inotify monitorEvents path
          -- TODO: use correct encoding here
          $ \e -> runMonitor env $ do
            handleEvent path (view tls env) rs e
        Log.debug $
          Log.msg ("watching file" :: Text)
            . Log.field "descriptor" (show wd)
            . Log.field "file" path
        pure wd
  Log.debug $
    Log.msg ("inotify initialized" :: Text)
      . Log.field "inotify" (show inotify)
  paths <- embed $ certificateWatchPaths rs
  traverse watch (toList paths)

handleEvent ::
  (Members '[TinyLog, Embed IO] r) =>
  RawFilePath ->
  MVar TLSSettings ->
  RunSettings ->
  Event ->
  Sem r ()
handleEvent watchedPath var rs e = case eventPath e of
  Nothing ->
    Log.warn $
      Log.msg ("unexpected monitor event" :: Text)
        . Log.field "event" (show e)
  Just path -> when (isRelevant path) $ do
    tls' <- embed $ mkTLSSettings rs
    Log.info $ Log.msg ("updating TLS settings" :: Text)
    embed @IO $ print tls'
    embed @IO $ modifyMVar_ var (const (pure tls'))
  where
    eventPath :: Event -> Maybe RawFilePath
    eventPath (Closed _ mpath True) = mpath <> Just watchedPath
    eventPath (MovedIn _ path _) = Just path
    eventPath (Created _ path) = Just path
    eventPath _ = Nothing

    isRelevant :: RawFilePath -> Bool
    isRelevant path | watchedPath == path = True
    isRelevant _path = False -- TODO

certificatePaths :: RunSettings -> [FilePath]
certificatePaths rs =
  maybeToList (remoteCAStore rs)
    ++ [ clientCertificate rs,
         clientPrivateKey rs
       ]

certificateWatchPaths :: RunSettings -> IO (Set RawFilePath)
certificateWatchPaths =
  fmap Set.fromList
    . traverse rawPath
    . (cert1 =<<)
    . certificatePaths
  where
    cert1 :: FilePath -> [FilePath]
    cert1 path = [path, takeDirectory path]

data FederationSetupError
  = InvalidCAStore FilePath
  | InvalidClientCertificate String
  deriving (Show)

instance Exception FederationSetupError

mkTLSSettings :: RunSettings -> IO TLSSettings
mkTLSSettings settings =
  TLSSettings
    <$> mkCAStore settings
    <*> mkCreds settings

mkCAStore :: RunSettings -> IO CertificateStore
mkCAStore settings = do
  customCAStore <- fmap (fromRight mempty) . Polysemy.runM . Polysemy.runError @() $ do
    path <- maybe (Polysemy.throw ()) pure $ remoteCAStore settings
    Polysemy.embed $ readCertificateStore path >>= maybe (throw $ InvalidCAStore path) pure
  systemCAStore <-
    if useSystemCAStore settings
      then getSystemCertificateStore
      else pure mempty
  pure (customCAStore <> systemCAStore)

mkCreds :: RunSettings -> IO TLS.Credential
mkCreds settings =
  handle h $
    TLS.credentialLoadX509 (clientCertificate settings) (clientPrivateKey settings)
      >>= \case
        Left e -> throw (InvalidClientCertificate e)
        Right (X509.CertificateChain [], _) ->
          throw (InvalidClientCertificate "could not read client certificate")
        Right x -> pure x
  where
    h :: IOException -> IO a
    h = throw . InvalidClientCertificate . show
