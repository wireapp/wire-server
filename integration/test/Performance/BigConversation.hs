{-# OPTIONS_GHC -Wwarn #-}

module Performance.BigConversation where

import API.BrigCommon
import API.Galley (getConversation)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.List.Extra (chunksOf)
import qualified Data.Text.Encoding as Text
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import MLS.Util
import SetupHelpers
import qualified System.CryptoBox as Cryptobox
import Testlib.Prelude
import UnliftIO (modifyIORef', newIORef, pooledMapConcurrentlyN, readIORef)
import UnliftIO.Temporary
import Prelude (writeFile)

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
batchForSize Big = 250
batchForSize Large = 500
batchForSize VeryLarge = 500

testCreateBigMLSConversation :: App ()
testCreateBigMLSConversation = do
  domain <- OwnDomain & asString
  let teamSize = 11
  let batchSize = 20
  let clientNotifCapability = Consumable
  putStrLn $ "Creating a team with " <> show teamSize <> " members"
  (owner, ownerClient, _, members, c1 : c2 : _) <- createTeamAndClients domain clientNotifCapability teamSize
  putStrLn $ "Creating a conversation with " <> show teamSize <> " members in batches of " <> show batchSize
  convSize <- liftIO $ newIORef (1 :: Int)
  (convId, totalTime) <-
    timeIt do
      convId <- createNewGroup def ownerClient
      let memberChunks = chunksOf batchSize members
      for_ memberChunks $ \chunk -> do
        (size, time) <- timeIt $ do
          msg <- createAddCommit ownerClient convId chunk
          void $ case clientNotifCapability of
            Legacy -> sendAndConsumeCommitBundle msg
            Consumable -> sendAndConsumeCommitBundleNew msg
          pure (BS.length msg.message)
        cs <- liftIO $ readIORef convSize
        putStrLn $ "Sent " <> show size <> " bytes in " <> show time <> ", adding " <> show (length chunk) <> " members to conv of size: " <> show cs
        liftIO $ modifyIORef' convSize (+ (length chunk))
        pure (size, time)
      pure convId
  putStrLn $ "Total time: " <> show totalTime
  do
    conv <- getConversation owner (convIdToQidObject convId) >>= getJSON 200
    otherMembers <- conv %. "members.others" & asList
    length otherMembers `shouldMatchInt` (teamSize - 1)
  (bytes, timeRemoval) <- timeIt $ do
    commit <- createRemoveCommit ownerClient convId [c1, c2]
    -- m <- showMessage def ownerClient commit.message
    -- prettyJSON m >>= liftIO . writeFile "removal.json"
    case clientNotifCapability of
      Legacy -> void $ sendAndConsumeCommitBundle commit
      Consumable -> void $ sendAndConsumeCommitBundleNew commit
    pure (BS.length commit.message)
  putStrLn $ "Sent " <> show bytes <> " bytes in " <> show timeRemoval <> " for removing 2 members"
  do
    conv <- getConversation owner (convIdToQidObject convId) >>= getJSON 200
    otherMembers <- conv %. "members.others" & asList
    length otherMembers `shouldMatchInt` (teamSize - 3)

timeIt :: App a -> App (a, NominalDiffTime)
timeIt action = do
  start <- liftIO getCurrentTime
  result <- action
  end <- liftIO getCurrentTime
  pure (result, diffUTCTime end start)

createTeamAndClients :: String -> ClientNotifCapability -> Int -> App (Value, ClientIdentity, String, [Value], [ClientIdentity])
createTeamAndClients domain clientNotifCapability teamSize = do
  (owner, tid, members) <- createTeam domain teamSize
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
                        lastPrekey = Just lastPrekey,
                        acapabilities = case clientNotifCapability of
                          Legacy -> Nothing
                          Consumable -> Just ["consumable-notifications"]
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

data ClientNotifCapability = Legacy | Consumable
