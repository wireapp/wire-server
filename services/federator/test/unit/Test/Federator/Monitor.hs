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
import Control.Monad.Trans.Cont
import Federator.Env (TLSSettings (..))
import Federator.Monitor.Internal
import Federator.Options
import Imports
import Polysemy (Embed, Member, Sem)
import qualified Polysemy
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.TinyLog as Polysemy
import System.IO.Temp
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
      testMonitorErrorLog
    ]

assertNoErrors ::
  (Exception e, Member (Embed IO) r) =>
  Sem (Polysemy.Error e ': r) a ->
  Sem r a
assertNoErrors m =
  Polysemy.runError m >>= \case
    Left e ->
      Polysemy.embed $
        assertFailure ("unexpected error: " ++ displayException e)
    Right x -> pure x

tempFile :: FilePath -> String -> ContT r IO FilePath
tempFile dir template =
  ContT $ \k -> withTempFile dir template (const . k)

withSettings :: ContT r IO RunSettings
withSettings = do
  dir <- liftIO $ getCanonicalTemporaryDirectory
  cert <- tempFile dir "cert.pem"
  liftIO $ copyFile "test/resources/unit/localhost.pem" cert
  key <- tempFile dir "key.pem"
  liftIO $ copyFile "test/resources/unit/localhost-key.pem" key
  pure $ defRunSettings cert key

withSilentMonitor :: ContT r IO (MVar TLSSettings, RunSettings)
withSilentMonitor = do
  settings <- withSettings
  tlsVar <- liftIO $ newMVar (error "TLSSettings not updated before being read")
  void . ContT $
    bracket
      (runSem (monitorCertificates runSemE tlsVar settings))
      (runSem . stopMonitoringCertificates)
  pure (tlsVar, settings)
  where
    runSem = Polysemy.runM . Polysemy.discardLogs
    runSemE = runSem . assertNoErrors @FederationSetupError

testMonitorChangeUpdate :: TestTree
testMonitorChangeUpdate =
  testCase "monitor updates settings on file change" $ do
    _tls <- evalContT $ do
      (tlsVar, settings) <- withSilentMonitor
      liftIO $
        copyFile
          "test/resources/unit/localhost-dot.pem"
          (clientCertificate settings)
      readMVar tlsVar
    pure ()

testMonitorOverwriteUpdate :: TestTree
testMonitorOverwriteUpdate = testCase "monitor updates settings on file change" $ pure ()

testMonitorSymlinkUpdate :: TestTree
testMonitorSymlinkUpdate = testCase "monitor updates settings on file change" $ pure ()

testMonitorErrorLog :: TestTree
testMonitorErrorLog = testCase "monitor updates settings on file change" $ pure ()
