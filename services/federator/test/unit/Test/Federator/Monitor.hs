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

module Test.Federator.Monitor (tests) where

import Control.Exception (bracket)
import Control.Lens (view)
import Control.Monad.Trans.Cont
import Data.X509 (CertificateChain (..))
import Federator.Env (TLSSettings (..), creds)
import Federator.Monitor
import Federator.Monitor.Internal
import Federator.Options
import Imports
import qualified Polysemy
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.TinyLog as Polysemy
import System.FilePath
import System.IO.Temp
import System.Posix (createSymbolicLink, getWorkingDirectory)
import System.Timeout
import Test.Federator.Options (defRunSettings)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Federator.Monitor"
    [ testMonitorChangeUpdate,
      testMonitorOverwriteUpdate,
      testMonitorSymlinkUpdate,
      testMonitorError
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

withSilentMonitor ::
  MVar (Maybe FederationSetupError) ->
  RunSettings ->
  ContT r IO (IORef TLSSettings)
withSilentMonitor done settings = do
  tlsVar <- liftIO $ newIORef (error "TLSSettings not updated before being read")
  void . ContT $
    bracket
      (runSem (monitorCertificates runSemE tlsVar settings))
      (runSem . stopMonitoringCertificates)
  pure tlsVar
  where
    runSem = Polysemy.runM . Polysemy.discardLogs
    runSemE action = do
      r <- runSem (Polysemy.runError @FederationSetupError action)
      void $ tryPutMVar done (either Just (const Nothing) r)

testMonitorChangeUpdate :: TestTree
testMonitorChangeUpdate =
  testCase "monitor updates settings on file change" $ do
    done <- newEmptyMVar
    evalContT $ do
      settings <- withSettings
      tlsVar <- withSilentMonitor done settings
      liftIO $ do
        appendFile (clientCertificate settings) ""
        result <- timeout 100000 (readMVar done)
        case result of
          Nothing -> assertFailure "certificate not updated within the allotted time"
          Just (Just err) ->
            assertFailure
              ("unexpected exception " <> displayException err)
          _ -> pure ()
        tls <- readIORef tlsVar
        case view creds tls of
          (CertificateChain [], _) ->
            assertFailure "expected non-empty certificate chain"
          _ -> pure ()

testMonitorOverwriteUpdate :: TestTree
testMonitorOverwriteUpdate =
  testCase "monitor updates settings on file being replaced" $ do
    done <- newEmptyMVar
    evalContT $ do
      settings <- withSettings
      tlsVar <- withSilentMonitor done settings
      liftIO $ do
        copyFile
          "test/resources/unit/localhost-dot.pem"
          (clientCertificate settings)
        result <- timeout 100000 (readMVar done)
        case result of
          Nothing -> assertFailure "certificate not updated within the allotted time"
          Just (Just err) ->
            assertFailure
              ("unexpected exception " <> displayException err)
          _ -> pure ()
        tls <- readIORef tlsVar
        case view creds tls of
          (CertificateChain [], _) ->
            assertFailure "expected non-empty certificate chain"
          _ -> pure ()

testMonitorSymlinkUpdate :: TestTree
testMonitorSymlinkUpdate =
  testCase "monitor updates settings symlink swap" $ do
    done <- newEmptyMVar
    evalContT $ do
      settings <- withSymlinkSettings
      tlsVar <- withSilentMonitor done settings
      liftIO $ do
        removeFile (clientCertificate settings)
        wd <- getWorkingDirectory
        createSymbolicLink
          (wd </> "test/resources/unit/localhost-dot.pem")
          (clientCertificate settings)
        result <- timeout 100000 (readMVar done)
        case result of
          Nothing -> assertFailure "certificate not updated within the allotted time"
          Just (Just err) ->
            assertFailure
              ("unexpected exception " <> displayException err)
          _ -> pure ()
        tls <- readIORef tlsVar
        case view creds tls of
          (CertificateChain [], _) ->
            assertFailure "expected non-empty certificate chain"
          _ -> pure ()

testMonitorError :: TestTree
testMonitorError =
  testCase "monitor returns an error when settings cannot be updated" $ do
    done <- newEmptyMVar
    evalContT $ do
      settings <- withSettings
      _ <- withSilentMonitor done settings
      liftIO $ do
        writeFile (clientCertificate settings) "not a certificate"
        result <- timeout 1000000 (readMVar done)
        case result of
          Nothing -> assertFailure "no error returned within the allotted time"
          Just Nothing -> assertFailure "unexpected success"
          _ -> pure ()
