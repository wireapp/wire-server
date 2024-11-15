{-# OPTIONS_GHC -Wwarn #-}

module Performance.BigConversation where

import API.BrigCommon
import Criterion
import Criterion.Main.Options
import Criterion.Types
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as Text
import MLS.Util
import SetupHelpers
import qualified System.CryptoBox as Cryptobox
import Testlib.Prelude
import UnliftIO (pooledMapConcurrentlyN)
import UnliftIO.Temporary
import Criterion.Main (runMode)
import Options.Applicative.Common (evalParser)
import Control.Monad.Reader (MonadReader(ask))

testCreateBigMLSConversation :: App ()
testCreateBigMLSConversation = do
  let teamSize = 20
  let clientsPerUser = 1
  (owner, _tid, members) <- createTeam OwnDomain teamSize
  let genPrekeyInBox box i = do
        pk <- assertCrytoboxSuccess =<< liftIO (Cryptobox.newPrekey box i)
        pkBS <- liftIO $ Cryptobox.copyBytes pk.prekey
        pure $ object ["id" .= i, "key" .= Text.decodeUtf8 (B64.encode pkBS)]
      genPrekeys = do
        withSystemTempDirectory "cryptobox-prekey-gen" $ \cryptoboxDir -> do
          box <- assertCrytoboxSuccess =<< liftIO (Cryptobox.open cryptoboxDir)
          firstPrekey <- genPrekeyInBox box 0
          lastPrekey <- genPrekeyInBox box maxBound
          pure (firstPrekey, lastPrekey)
      createClient user = do
        (firstPrekey, lastPrekey) <- genPrekeys
        let mlsClientOpts =
              def
                { clientArgs =
                    def
                      { prekeys = Just [firstPrekey],
                        lastPrekey = Just lastPrekey
                      }
                }
        createMLSClient def mlsClientOpts user
  ownerClient <- createClient owner
  _memClients <- pooledMapConcurrentlyN 64 (replicateM_ clientsPerUser . createClient) members
  let createConv n = do
        convId <- createNewGroup def ownerClient
        void $ sendAndConsumeCommitBundle =<< createAddCommit ownerClient convId (take n members)
  let conf = defaultConfig { reportFile = Just $ "big-conversation-" <> show clientsPerUser <> "-clients-per-user.html" }
  case evalParser $ parseWith conf of
    Nothing -> assertFailure "Failed to parse criterion options"
    Just mode -> do
      e <- ask
      let mkBenchmark n = bench ("conversation with " <> show n <> " members") $ nfIO (runAppWithEnv e $ createConv n)
      let benchmarks = mkBenchmark <$> [10]
      liftIO $ runMode mode benchmarks

assertCrytoboxSuccess :: (Show a) => Cryptobox.Result a -> App a
assertCrytoboxSuccess = \case
  Cryptobox.Success x -> pure x
  e -> assertFailure $ "Cryptobox exception: " <> show e
