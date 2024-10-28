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

testCreateBigMLSConversation :: App ()
testCreateBigMLSConversation = do
  (owner, _tid, members) <- createTeam OwnDomain 2000
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
  _memClients <- pooledMapConcurrentlyN 64 createClient members
  createConv <- appToIO $ do
    convId <- createNewGroup def ownerClient
    void $ sendAndConsumeCommitBundle =<< createAddCommit ownerClient convId members
  let benchmarkable = toBenchmarkable (\n -> replicateM_ (fromIntegral n) createConv)
  liftIO $ benchmarkWith (defaultConfig {resamples = 5}) benchmarkable

assertCrytoboxSuccess :: (Show a) => Cryptobox.Result a -> App a
assertCrytoboxSuccess = \case
  Cryptobox.Success x -> pure x
  e -> assertFailure $ "Cryptobox exception: " <> show e
