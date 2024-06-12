-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
import Data.ByteString (packCStringLen, useAsCStringLen)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Federator.Options (RunSettings (..))
import GHC.Foreign (peekCStringLen, withCStringLen)
import GHC.IO.Encoding (getFileSystemEncoding)
import Imports
import OpenSSL.Session (SSLContext)
import OpenSSL.Session qualified as SSL
import Polysemy (Embed, Member, Members, Sem, embed)
import Polysemy qualified
import Polysemy.Error qualified as Polysemy
import Polysemy.Final (Final)
import Polysemy.Resource qualified as Polysemy
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import System.FilePath
import System.INotify
import System.Logger (Logger)
import System.Logger.Message qualified as Log
import System.Posix.ByteString (RawFilePath)
import System.Posix.Files
import Wire.Arbitrary
import Wire.Sem.Logger.TinyLog qualified as Log

data Monitor = Monitor
  { monINotify :: INotify,
    monOnNewContext :: SSLContext -> IO (),
    monWatches :: IORef Watches,
    monSettings :: RunSettings,
    monHandler :: WatchedPath -> Event -> IO (),
    monLock :: MVar ()
  }

-- This is needed because the normal Posix file system API uses strings, while
-- the inotify API uses bytestrings.
-- /Note/: File paths are strings obtained using the "file system encoding",
-- which is the same as the locale encoding, but uses some escaping tricks to
-- be able to represent arbitrary data as strings.
rawPath :: FilePath -> IO RawFilePath
rawPath path = do
  encoding <- getFileSystemEncoding
  withCStringLen encoding path packCStringLen

fromRawPath :: RawFilePath -> IO FilePath
fromRawPath path = do
  encoding <- getFileSystemEncoding
  useAsCStringLen path (peekCStringLen encoding)

data WatchedPath
  = WatchedFile RawFilePath
  | WatchedDir RawFilePath (Set RawFilePath)
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform WatchedPath)

mergePaths :: [WatchedPath] -> Set WatchedPath
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

watchPathEvents :: WatchedPath -> [EventVariety]
watchPathEvents (WatchedFile _) = [CloseWrite]
watchPathEvents (WatchedDir _ _) = [MoveIn, Create]

-- Since we are watching a filesystem path, and not an inode, we need to replace a
-- file watch when the file gets overwritten.
-- This type is a map of paths to watches used to keep track of both file and
-- directory watches as they get deleted and recreated.
type Watches = Map RawFilePath (WatchDescriptor, WatchedPath)

runSemDefault :: Logger -> Sem '[TinyLog, Embed IO, Final IO] a -> IO a
runSemDefault logger = Polysemy.runFinal . Polysemy.embedToFinal . Log.loggerToTinyLog logger

delMonitor ::
  ( Member TinyLog r,
    Member (Embed IO) r,
    Member (Final IO) r
  ) =>
  Monitor ->
  Sem r ()
delMonitor monitor = Polysemy.resourceToIOFinal
  $ Polysemy.bracket
    (takeMVar (monLock monitor))
    (putMVar (monLock monitor))
    . const
  $ do
    watches <- readIORef (monWatches monitor)
    traverse_ stop watches
  where
    stop (wd, _) = do
      -- ignore exceptions when removing watches
      embed . void . try @IOException $ removeWatch wd
      Log.trace $
        Log.msg ("stopped watching file" :: Text)
          . Log.field "descriptor" (show wd)

mkMonitor ::
  ( Member TinyLog r,
    Member (Embed IO) r,
    Member TinyLog r1,
    Member (Embed IO) r1,
    Member (Polysemy.Error FederationSetupError) r1
  ) =>
  (Sem r1 () -> IO ()) ->
  (SSLContext -> IO ()) ->
  RunSettings ->
  Sem r Monitor
mkMonitor runSem onNewContext rs = do
  inotify <- embed initINotify
  Log.trace $
    Log.msg ("inotify initialized" :: Text)
      . Log.field "inotify" (show inotify)

  lock <- embed @IO $ newMVar ()
  watchesVar <- embed @IO $ newIORef mempty

  let monitor =
        Monitor
          { monINotify = inotify,
            monOnNewContext = onNewContext,
            monWatches = watchesVar,
            monSettings = rs,
            monHandler = handleEvent runSem monitor,
            monLock = lock
          }

  paths <- embed $ certificateWatchPaths rs
  traverse_ (addWatchedFile monitor) (toList paths)
  pure monitor

data Action = ReplaceWatch RawFilePath | ReloadSettings
  deriving (Eq, Ord, Show)

handleEvent ::
  ( Member TinyLog r,
    Member (Embed IO) r,
    Member (Polysemy.Error FederationSetupError) r
  ) =>
  (Sem r () -> IO ()) ->
  Monitor ->
  WatchedPath ->
  Event ->
  IO ()
handleEvent runSem monitor wpath e = do
  let actions = getActions wpath e
  -- only use runSem when there are some actions
  -- this makes it possible to use a special runSem in the tests that is able
  -- to detect when some action has taken place
  unless (null actions) $
    -- we take the lock here, so that handlers never execute concurrently
    withMVar (monLock monitor) $ \_ ->
      runSem $ traverse_ (applyAction monitor) actions

-- Note: it is important that the watch is replaced *before* settings are
-- reloaded, otherwise there is a window of time (after reloading settings,
-- but before the new watch is set) where changes to the settings can go
-- undetected
getActions :: WatchedPath -> Event -> [Action]
getActions (WatchedFile path) (Closed _ mpath True)
  | maybe True (== path) mpath = [ReloadSettings]
getActions (WatchedDir dir paths) (MovedIn _ path _)
  | Set.member path paths = [ReplaceWatch (dir <> "/" <> path), ReloadSettings]
getActions (WatchedDir dir paths) (Created _ path)
  | Set.member path paths = [ReplaceWatch (dir <> "/" <> path), ReloadSettings]
getActions _ _ = []

applyAction ::
  ( Member TinyLog r,
    Member (Embed IO) r,
    Member (Polysemy.Error FederationSetupError) r
  ) =>
  Monitor ->
  Action ->
  Sem r ()
applyAction monitor ReloadSettings = do
  sslCtx' <- mkSSLContext (monSettings monitor)
  Log.info $ Log.msg ("updating TLS settings" :: Text)
  embed @IO $ monOnNewContext monitor sslCtx'
applyAction monitor (ReplaceWatch path) = do
  watches <- readIORef (monWatches monitor)
  case Map.lookup path watches of
    Nothing -> pure ()
    Just (_, wpath) -> do
      addWatchedFile monitor wpath
      case wpath of
        WatchedDir dir paths ->
          traverse_ (applyAction monitor . ReplaceWatch . ((dir <> "/") <>)) paths
        WatchedFile _ -> pure ()

addWatchedFile ::
  ( Member TinyLog r,
    Member (Embed IO) r
  ) =>
  Monitor ->
  WatchedPath ->
  Sem r ()
addWatchedFile monitor wpath = do
  r <-
    embed . try @SomeException $
      addWatchAndSave
        (monINotify monitor)
        (watchPathEvents wpath)
        (monWatches monitor)
        wpath
        (monHandler monitor wpath)
  let pathText = Text.decodeUtf8With Text.lenientDecode (watchedPath wpath)
  case r of
    Right w ->
      Log.trace $
        Log.msg ("watching file" :: Text)
          . Log.field "descriptor" (show w)
          . Log.field "path" pathText
    Left e -> do
      Log.err $
        Log.msg ("error while try to add file watch" :: Text)
          . Log.field "path" pathText
          . Log.field "error" (displayException e)

addWatchAndSave ::
  INotify ->
  [EventVariety] ->
  IORef Watches ->
  WatchedPath ->
  (Event -> IO ()) ->
  IO WatchDescriptor
addWatchAndSave inotify events watchesVar wpath handler = do
  let path = watchedPath wpath
  -- create a new watch
  w' <- addWatch inotify events path handler
  -- atomically save it in the map, and return the old one
  mw <-
    atomicModifyIORef watchesVar $
      swap . Map.alterF (,Just (w', wpath)) path
  -- remove the old watch
  case mw of
    Nothing -> pure ()
    Just (w, _) -> void . try @IOException $ removeWatch w
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
    . traverse (watchedPaths resolveSymlink)
    . certificatePaths

resolveSymlink :: FilePath -> IO (Maybe FilePath)
resolveSymlink path' = do
  let path = dropTrailingPathSeparator path'
  status <- getSymbolicLinkStatus path
  if isSymbolicLink status
    then do
      target <- readSymbolicLink path
      pure . Just $
        if isRelative target
          then takeDirectory path </> target
          else target
    else pure Nothing

watchedPaths :: (FilePath -> IO (Maybe FilePath)) -> FilePath -> IO [WatchedPath]
watchedPaths resolve path' = do
  path <- makeAbsolute path'
  rpath <- rawPath path
  dirs <- watchedDirs resolve path
  pure $ WatchedFile rpath : dirs

watchedDirs :: (FilePath -> IO (Maybe FilePath)) -> FilePath -> IO [WatchedPath]
watchedDirs resolve path = do
  dirs0 <- resolve path >>= maybe (pure []) (watchedDirs resolve)
  let (dir, base) = splitFileName (dropTrailingPathSeparator path)
  dirs1 <-
    if dir == path
      then pure [] -- base case: root directory
      else do
        wds <- watchedDirs resolve dir
        rdir <- rawPath (dropTrailingPathSeparator dir)
        rbase <- rawPath base
        pure $ WatchedDir rdir (Set.singleton rbase) : wds
  pure (dirs0 ++ dirs1)

data FederationSetupError
  = InvalidCAStore FilePath String
  | InvalidClientCertificate String
  | InvalidClientPrivateKey String
  | CertificateAndPrivateKeyDoNotMatch FilePath FilePath
  | SSLException SSL.SomeSSLException
  deriving (Show)

instance Exception FederationSetupError

showFederationSetupError :: FederationSetupError -> Text
showFederationSetupError (InvalidCAStore path msg) = "invalid CA store: " <> Text.pack path <> ", error: " <> Text.pack msg
showFederationSetupError (InvalidClientCertificate msg) = Text.pack msg
showFederationSetupError (InvalidClientPrivateKey msg) = Text.pack msg
showFederationSetupError (CertificateAndPrivateKeyDoNotMatch cert key) = Text.pack $ "Certificate and private key do not match, certificate: " <> cert <> ", private key: " <> key
showFederationSetupError (SSLException exc) = Text.pack $ "Unexpected SSL Exception: " <> displayException exc

mkSSLContext ::
  ( Member (Embed IO) r,
    Member (Polysemy.Error FederationSetupError) r
  ) =>
  RunSettings ->
  Sem r SSLContext
mkSSLContext settings = do
  ctx <- mkSSLContextWithoutCert settings

  Polysemy.fromExceptionVia @SomeException (InvalidClientCertificate . displayException) $
    SSL.contextSetCertificateChainFile ctx (clientCertificate settings)

  Polysemy.fromExceptionVia @SomeException (InvalidClientPrivateKey . displayException) $
    SSL.contextSetPrivateKeyFile ctx (clientPrivateKey settings)

  privateKeyCheck <- Polysemy.fromExceptionVia @SSL.SomeSSLException SSLException $ SSL.contextCheckPrivateKey ctx
  unless privateKeyCheck $ do
    Polysemy.throw $ CertificateAndPrivateKeyDoNotMatch (clientCertificate settings) (clientPrivateKey settings)

  pure ctx

mkSSLContextWithoutCert :: Members '[Embed IO, Polysemy.Error FederationSetupError] r => RunSettings -> Sem r SSLContext
mkSSLContextWithoutCert settings = do
  ctx <- embed $ SSL.context
  embed $ do
    SSL.contextAddOption ctx SSL.SSL_OP_ALL
    SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv2
    SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv3
    SSL.contextAddOption ctx SSL.SSL_OP_NO_TLSv1

    -- Settings TLS13 ciphers requires another call to openssl, this has not
    -- been implemented in HsOpenSSL yet.
    SSL.contextSetCiphers ctx blessedTLS12Ciphers

    SSL.contextSetALPNProtos ctx ["h2"]

    SSL.contextSetVerificationMode ctx $
      SSL.VerifyPeer
        { -- vpFailIfNoPeerCert and vpClientOnce are only relevant for servers
          SSL.vpFailIfNoPeerCert = False,
          SSL.vpClientOnce = False,
          SSL.vpCallback = Nothing
        }
  forM_ (remoteCAStore settings) $ \caStorePath ->
    Polysemy.fromExceptionVia @SomeException (InvalidCAStore caStorePath . displayException) $
      SSL.contextSetCAFile ctx caStorePath

  when (useSystemCAStore settings) $
    embed (SSL.contextSetDefaultVerifyPaths ctx)

  pure ctx

-- Context and possible future work see
-- https://wearezeta.atlassian.net/browse/FS-33
-- https://wearezeta.atlassian.net/browse/FS-444
-- https://wearezeta.atlassian.net/browse/FS-443
--
-- The current list is compliant with TR-02102-2
-- https://www.bsi.bund.de/SharedDocs/Downloads/EN/BSI/Publications/TechGuidelines/TG02102/BSI-TR-02102-2.html
blessedTLS12Ciphers :: String
blessedTLS12Ciphers =
  intercalate
    ":"
    [ -- For TLS 1.2 (copied from nginx ingress config):
      "ECDHE-ECDSA-AES256-GCM-SHA384",
      "ECDHE-RSA-AES256-GCM-SHA384"
    ]
