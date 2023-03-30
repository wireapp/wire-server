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

module Test.Federator.Monitor (tests) where

import Control.Concurrent.Chan
import Control.Exception (bracket)
import Control.Monad.Trans.Cont
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Set as Set
import Federator.Monitor
import Federator.Monitor.Internal
import Federator.Options
import Imports
import OpenSSL.Session (SSLContext)
import qualified Polysemy
import qualified Polysemy.Error as Polysemy
import System.FilePath
import System.IO.Temp
import System.Posix (createSymbolicLink, getWorkingDirectory)
import System.Timeout
import Test.Federator.Options (defRunSettings)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Wire.Sem.Logger.TinyLog as Log

timeoutMicroseconds :: Int
timeoutMicroseconds = 10000000

tests :: TestTree
tests =
  testGroup
    "Federator.Monitor"
    [ testMonitorChangeUpdate,
      testMonitorReplacedChangeUpdate,
      testMonitorOverwriteUpdate,
      testMonitorSymlinkUpdate,
      testMonitorNestedUpdate,
      testMonitorKubernetesUpdate,
      testMonitorDeepUpdate,
      testMonitorError,
      testMergeWatchedPaths,
      testDirectoryTraversal
    ]

tempFile :: FilePath -> String -> ContT r IO FilePath
tempFile dir template =
  ContT $ \k -> withTempFile dir template (const . k)

withSettings :: ContT r IO RunSettings
withSettings = do
  dir <- liftIO getCanonicalTemporaryDirectory
  cert <- tempFile dir "cert.pem"
  liftIO $ copyFile "test/resources/unit/localhost.pem" cert
  key <- tempFile dir "key.pem"
  liftIO $ copyFile "test/resources/unit/localhost-key.pem" key
  pure $ defRunSettings cert key

withSymlinkSettings :: ContT r IO RunSettings
withSymlinkSettings = do
  settings <- withSettings
  dir <- ContT $ withSystemTempDirectory "conf"
  liftIO $ createSymbolicLink (clientCertificate settings) (dir </> "cert.pem")
  liftIO $ createSymbolicLink (clientPrivateKey settings) (dir </> "key.pem")
  pure $
    settings
      { clientCertificate = dir </> "cert.pem",
        clientPrivateKey = dir </> "key.pem"
      }

withNestedSettings :: Int -> ContT r IO RunSettings
withNestedSettings n = do
  root <- ContT $ withSystemTempDirectory "conf"
  liftIO $ do
    forM_ [1 .. n] $ \i -> do
      let path = concat ["d" ++ show j ++ "/" | j <- [1 .. i]]
      createDirectory (root </> path)
    let dir = root </> concat ["d" ++ show j ++ "/" | j <- [1 .. n]]
        cert = dir </> "cert.pem"
        key = dir </> "key.pem"
    copyFile "test/resources/unit/localhost.pem" cert
    copyFile "test/resources/unit/localhost-key.pem" key
    pure $ defRunSettings cert key

withKubernetesSettings :: ContT r IO RunSettings
withKubernetesSettings = do
  root <- ContT $ withSystemTempDirectory "secrets"
  liftIO $ do
    createDirectory (root </> "..foo")
    copyFile "test/resources/unit/localhost.pem" (root </> "..foo/cert.pem")
    copyFile "test/resources/unit/localhost-key.pem" (root </> "..foo/key.pem")

    createSymbolicLink (root </> "..foo") (root </> "..data")
    createSymbolicLink (root </> "..data/cert.pem") (root </> "cert.pem")
    createSymbolicLink (root </> "..data/key.pem") (root </> "key.pem")
    pure $ defRunSettings (root </> "cert.pem") (root </> "key.pem")

withSilentMonitor ::
  Chan (Maybe FederationSetupError) ->
  RunSettings ->
  ContT r IO (IORef SSLContext)
withSilentMonitor reloads settings = do
  tlsVar <- liftIO $ newIORef (error "TLSSettings not updated before being read")
  void . ContT $
    bracket
      (runSem (mkMonitor runSemE tlsVar settings))
      (runSem . delMonitor)
  pure tlsVar
  where
    runSem = Polysemy.runFinal . Polysemy.embedToFinal @IO . Log.discardTinyLogs
    runSemE action = do
      r <- runSem (Polysemy.runError @FederationSetupError action)
      writeChan reloads (either Just (const Nothing) r)

testMonitorChangeUpdate :: TestTree
testMonitorChangeUpdate =
  testCase "monitor updates settings on file change" $ do
    reloads <- newChan
    evalContT $ do
      settings <- withSettings
      _ <- withSilentMonitor reloads settings
      liftIO $ do
        appendFile (clientCertificate settings) ""
        result <- timeout timeoutMicroseconds (readChan reloads)
        case result of
          Nothing -> assertFailure "certificate not updated within the allotted time"
          Just (Just err) ->
            assertFailure
              ("unexpected exception " <> displayException err)
          _ -> pure ()

testMonitorReplacedChangeUpdate :: TestTree
testMonitorReplacedChangeUpdate =
  testCase "monitor updates settings on file changed after being replaced" $ do
    reloads <- newChan
    evalContT $ do
      settings <- withSettings
      _ <- withSilentMonitor reloads settings
      liftIO $ do
        -- first replace file with a different one
        copyFile
          "test/resources/unit/localhost-dot.pem"
          (clientCertificate settings)
        -- This will always fail because now the certificate doesn't match the
        -- private key
        _result0 <- timeout timeoutMicroseconds (readChan reloads)
        copyFile
          "test/resources/unit/localhost-dot-key.pem"
          (clientPrivateKey settings)
        result1 <- timeout timeoutMicroseconds (readChan reloads)
        case result1 of
          Nothing ->
            assertFailure
              "certificate not updated once within the allotted time"
          Just (Just err) ->
            assertFailure
              ("unexpected exception " <> displayException err)
          _ -> pure ()
        -- now modify the replaced file
        appendFile (clientCertificate settings) ""
        result2 <- timeout timeoutMicroseconds (readChan reloads)
        case result2 of
          Nothing ->
            assertFailure
              "certificate not updated twice within the allotted time"
          Just (Just err) ->
            assertFailure
              ("unexpected exception " <> displayException err)
          _ -> pure ()

testMonitorOverwriteUpdate :: TestTree
testMonitorOverwriteUpdate =
  testCase "monitor updates settings on file being replaced" $ do
    reloads <- newChan
    evalContT $ do
      settings <- withSettings
      _ <- withSilentMonitor reloads settings
      liftIO $ do
        copyFile
          "test/resources/unit/localhost-dot.pem"
          (clientCertificate settings)
        -- This will always fail because now the certificate doesn't match the
        -- private key
        _result0 <- timeout timeoutMicroseconds (readChan reloads)

        copyFile
          "test/resources/unit/localhost-dot-key.pem"
          (clientPrivateKey settings)
        result <- timeout timeoutMicroseconds (readChan reloads)
        case result of
          Nothing -> assertFailure "certificate not updated within the allotted time"
          Just (Just err) ->
            assertFailure
              ("unexpected exception " <> displayException err)
          _ -> pure ()

testMonitorSymlinkUpdate :: TestTree
testMonitorSymlinkUpdate =
  testCase "monitor updates settings symlink swap" $ do
    reloads <- newChan
    evalContT $ do
      settings <- withSymlinkSettings
      _ <- withSilentMonitor reloads settings
      liftIO $ do
        wd <- getWorkingDirectory

        removeFile (clientCertificate settings)
        createSymbolicLink
          (wd </> "test/resources/unit/localhost-dot.pem")
          (clientCertificate settings)
        -- This will always fail because now the certificate doesn't match the
        -- private key
        _result0 <- timeout timeoutMicroseconds (readChan reloads)

        removeFile (clientPrivateKey settings)
        createSymbolicLink
          (wd </> "test/resources/unit/localhost-dot-key.pem")
          (clientPrivateKey settings)
        result <- timeout timeoutMicroseconds (readChan reloads)
        case result of
          Nothing -> assertFailure "certificate not updated within the allotted time"
          Just (Just err) ->
            assertFailure
              ("unexpected exception " <> displayException err)
          _ -> pure ()

testMonitorNestedUpdate :: TestTree
testMonitorNestedUpdate =
  testCase "monitor updates when parent directory is replaced" $ do
    reloads <- newChan
    evalContT $ do
      settings <- withNestedSettings 1
      _ <- withSilentMonitor reloads settings
      liftIO $ do
        -- make a new directory with other credentials
        let parent = takeDirectory (clientCertificate settings)
            root = takeDirectory parent
        createDirectory (root </> "a1")
        let cert = root </> "a1/cert.pem"
            key = root </> "a1/key.pem"
        copyFile "test/resources/unit/localhost-dot.pem" cert
        copyFile "test/resources/unit/localhost-dot-key.pem" key

        -- replace the old directory with the new one
        renameDirectory (root </> "d1") (root </> "b1")
        renameDirectory (root </> "a1") (root </> "d1")

        result <- timeout timeoutMicroseconds (readChan reloads)
        case result of
          Nothing -> assertFailure "certificate not updated within the allotted time"
          Just (Just err) ->
            assertFailure
              ("unexpected exception " <> displayException err)
          _ -> pure ()

testMonitorDeepUpdate :: TestTree
testMonitorDeepUpdate =
  testCase "monitor updates when grandparent directory is replaced" $ do
    reloads <- newChan
    evalContT $ do
      settings <- withNestedSettings 2
      _ <- withSilentMonitor reloads settings
      liftIO $ do
        -- make a new directory with other credentials
        let root = takeDirectory (takeDirectory (takeDirectory (clientCertificate settings)))
        createDirectory (root </> "a1")
        createDirectory (root </> "a1/d2")
        let cert = root </> "a1/d2/cert.pem"
            key = root </> "a1/d2/key.pem"
        copyFile "test/resources/unit/localhost-dot.pem" cert
        copyFile "test/resources/unit/localhost-dot-key.pem" key

        -- replace the old directory with the new one
        renameDirectory (root </> "d1") (root </> "b1")
        renameDirectory (root </> "a1") (root </> "d1")

        timeout timeoutMicroseconds (readChan reloads) >>= \case
          Nothing -> assertFailure "certificate not updated once within the allotted time"
          Just (Just err) ->
            assertFailure
              ("unexpected exception " <> displayException err)
          _ -> pure ()

        -- test that further changes are seen
        appendFile (clientCertificate settings) ""
        timeout timeoutMicroseconds (readChan reloads) >>= \case
          Nothing -> assertFailure "certificate not updated twice within the allotted time"
          Just (Just err) ->
            assertFailure
              ("unexpected exception " <> displayException err)
          _ -> pure ()

testMonitorKubernetesUpdate :: TestTree
testMonitorKubernetesUpdate = do
  testCase "monitor updates on a kubernetes secret mount" $ do
    reloads <- newChan
    evalContT $ do
      settings <- withKubernetesSettings
      _ <- withSilentMonitor reloads settings
      liftIO $ do
        let root = takeDirectory (clientCertificate settings)
        createDirectory (root </> "..foo2")
        copyFile "test/resources/unit/localhost-dot.pem" (root </> "..foo2/cert.pem")
        copyFile "test/resources/unit/localhost-dot-key.pem" (root </> "..foo2/key.pem")

        removeFile (root </> "..data")
        createSymbolicLink (root </> "..foo2") (root </> "..data")

        timeout timeoutMicroseconds (readChan reloads) >>= \case
          Nothing -> assertFailure "certificate not updated once within the allotted time"
          Just (Just err) ->
            assertFailure
              ("unexpected exception " <> displayException err)
          _ -> pure ()

testMonitorError :: TestTree
testMonitorError =
  testCase "monitor returns an error when settings cannot be updated" $ do
    reloads <- newChan
    evalContT $ do
      settings <- withSettings
      _ <- withSilentMonitor reloads settings
      liftIO $ do
        writeFile (clientCertificate settings) "not a certificate"
        result <- timeout timeoutMicroseconds (readChan reloads)
        case result of
          Nothing -> assertFailure "no error returned within the allotted time"
          Just Nothing -> assertFailure "unexpected success"
          _ -> pure ()

testMergeWatchedPaths :: TestTree
testMergeWatchedPaths =
  testGroup
    "merged paths"
    [ testProperty "contain the same files" $ \(wpaths :: [WatchedPath]) ->
        let f (WatchedFile path) = [path]
            f (WatchedDir _ _) = []
            mergedFiles = Set.fromList (Set.toList (mergePaths wpaths) >>= f)
            origFiles = Set.fromList (wpaths >>= f)
         in mergedFiles == origFiles,
      testProperty "contain the same directories" $ \(wpaths :: [WatchedPath]) ->
        let f (WatchedFile _) = []
            f (WatchedDir dir _) = [dir]
            mergedDirs = Set.fromList (Set.toList (mergePaths wpaths) >>= f)
            origDirs = Set.fromList (wpaths >>= f)
         in mergedDirs == origDirs,
      testProperty "has no duplicated directories" $ \(wpaths :: [WatchedPath]) ->
        let f (WatchedFile _) = []
            f (WatchedDir dir _) = [dir]
            mergedDirList = Set.toList (mergePaths wpaths) >>= f
            mergedDirs = Set.fromList mergedDirList
         in Set.size mergedDirs == length mergedDirList,
      testProperty "has lower total count" $ \(wpaths :: [WatchedPath]) ->
        let f (WatchedFile _) = 1
            f (WatchedDir _ files) = Set.size files
            mergedCount = sum $ map f (Set.toList (mergePaths wpaths))
            origCount = sum (map f wpaths)
         in mergedCount <= origCount,
      testProperty "has the same paths" $ \(wpaths :: [WatchedPath]) ->
        let f (WatchedFile path) = [path]
            f (WatchedDir dir files) = map (dir <>) (Set.toList files)
            mergedPaths = Set.fromList (Set.toList (mergePaths wpaths) >>= f)
            origPaths = Set.fromList (wpaths >>= f)
         in mergedPaths == origPaths
    ]

newtype Path = Path {getRawPath :: ByteString}

getPath :: Path -> IO FilePath
getPath = fromRawPath . getRawPath

getAbsolutePath :: Path -> IO FilePath
getAbsolutePath p = do
  path <- getPath p
  makeAbsolute ("/" <> path)

instance Show Path where
  show = show . getRawPath

instance Arbitrary Path where
  arbitrary =
    Path . B8.intercalate "/"
      <$> listOf (BS.pack <$> listOf1 ch)
    where
      ch :: Gen Word8
      ch = arbitrary `suchThat` (/= fromIntegral (ord '/'))

trivialResolve :: FilePath -> IO (Maybe FilePath)
trivialResolve _ = pure Nothing

testDirectoryTraversal :: TestTree
testDirectoryTraversal =
  testGroup
    "directory traversal"
    [ testProperty "the number of entries is the same as the number of path components" $
        \(path' :: Path) -> ioProperty $ do
          path <- getAbsolutePath path'
          wpaths <- watchedPaths trivialResolve path
          pure (length wpaths == length (splitPath path)),
      testProperty "relative paths are resolved correctly" $
        \(path' :: Path) -> ioProperty $ do
          dir <- getWorkingDirectory
          path <- getPath path'
          wpaths <- watchedPaths trivialResolve path
          wpaths' <- watchedPaths trivialResolve (dir </> path)
          pure $ wpaths == wpaths',
      testCase "symlinked paths are resolved" $
        evalContT $ do
          settings <- withKubernetesSettings
          liftIO $ do
            rroot <- rawPath $ takeDirectory (clientCertificate settings)
            wpaths <- mergePaths <$> watchedPaths resolveSymlink (clientCertificate settings)
            assertBool "symlink targets should be watched" $
              Set.member
                (WatchedDir rroot (Set.fromList ["cert.pem", "..data", "..foo"]))
                wpaths
    ]
