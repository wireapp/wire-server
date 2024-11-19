{-# OPTIONS_GHC -Wwarn #-}

module Performance.BigConversation where

import API.BrigCommon
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.List.Extra (chunksOf)
import qualified Data.Text.Encoding as Text
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import MLS.Util
import SetupHelpers
import qualified System.CryptoBox as Cryptobox
import Testlib.Prelude
import UnliftIO (pooledMapConcurrentlyN)
import UnliftIO.Temporary

-- | A size saying how big an MLS conversation is. Each size is mapped to a
-- number via the 'sizeToNumber' function.
data ConversationSize
  = Tiny
  | Small
  | Medium
  | Big
  | Large
  | VeryLarge
  deriving (Eq, Generic, Show)

sizeToNumber :: ConversationSize -> Word
sizeToNumber Tiny = 20
sizeToNumber Small = 100
sizeToNumber Medium = 500
sizeToNumber Big = 1000
sizeToNumber Large = 5000
sizeToNumber VeryLarge = 10000

batchForSize :: ConversationSize -> Word
batchForSize Tiny = 10
batchForSize Small = 20
batchForSize Medium = 100
batchForSize Big = 100
batchForSize Large = 250
batchForSize VeryLarge = 500

testCreateBigMLSConversation :: ConversationSize -> App ()
testCreateBigMLSConversation convSize = do
  let teamSize = sizeToNumber convSize
  let batchSize = fromIntegral . batchForSize $ convSize
  totalTime <-
    fmap snd $ timeIt do
      (_, ownerClient, _, members, _) <- createTeamAndClients . fromIntegral $ teamSize
      convId <- createNewGroup def ownerClient
      let memberChunks = chunksOf batchSize members
      for_ memberChunks $ \chunk -> do
        (size, time) <- timeIt $ do
          msg <- createAddCommit ownerClient convId chunk
          void $ sendAndConsumeCommitBundle msg
          pure (BS.length msg.message)
        putStrLn $ "Sent " <> show size <> " bytes in " <> show time
        pure (size, time)
  putStrLn $ "Total time: " <> show totalTime

timeIt :: App a -> App (a, NominalDiffTime)
timeIt action = do
  start <- liftIO getCurrentTime
  result <- action
  end <- liftIO getCurrentTime
  pure (result, diffUTCTime end start)

createTeamAndClients :: Int -> App (Value, ClientIdentity, String, [Value], [ClientIdentity])
createTeamAndClients teamSize = do
  (owner, tid, members) <- createTeam OwnDomain teamSize
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
  memClients <- pooledMapConcurrentlyN 64 createClient members
  for_ memClients $ uploadNewKeyPackage def
  pure (owner, ownerClient, tid, members, memClients)

assertCrytoboxSuccess :: (Show a) => Cryptobox.Result a -> App a
assertCrytoboxSuccess = \case
  Cryptobox.Success x -> pure x
  e -> assertFailure $ "Cryptobox exception: " <> show e
