{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Catch (MonadCatch, SomeException (SomeException), catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import Data.Int (Int32)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, mapMaybe)
import Data.Semigroup (Last (..))
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V1 as UUIDv1
import qualified Data.UUID.V4 as UUIDv4
import Database.CQL.IO (MonadClient, PrepQuery, R, S, W)
import qualified Database.CQL.IO as C
import qualified Database.CQL.Protocol as C
import System.IO
import System.Posix (installHandler, sigUSR1)
import qualified System.Posix as Signal

logger :: C.Logger
logger =
  C.Logger
    { C.logMessage = \lvl bs -> putStrLn $ show lvl <> ": " <> LBSC8.unpack (toLazyByteString bs),
      C.logRequest = \_ -> pure (),
      C.logResponse = \_ -> pure ()
    }

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  clientState <-
    C.init $
      C.setLogger logger
        . C.setContacts "gundeck-gundeck-eks-service.databases" []
        . C.setPortNumber 9042
        . C.setKeyspace (C.Keyspace "gundeck")
        . C.setMaxConnections 1
        . C.setMaxStreams 1
        . C.setPoolStripes 1
        . C.setSendTimeout 3
        . C.setResponseTimeout 10
        . C.setProtocolVersion C.V4
        $ C.defSettings

  installHandler sigUSR1 (Signal.Catch $ C.runClient clientState printDebugInfo) Nothing
  C.runClient clientState $ do
    createTable
    writeAndReadLoop

printDebugInfo :: MonadClient m => m ()
printDebugInfo = do
  liftIO $ putStrLn "======= DEBUG INFO ======="
  liftIO . print =<< C.debugInfo
  liftIO $ putStrLn "======= DEBUG INFO ======="

writeAndReadLoop :: (MonadClient m, MonadCatch m) => m ()
writeAndReadLoop = do
  writeAndRead
    `catch` ( \(e :: SomeException) -> do
                liftIO $ putStrLn $ "Failure: " <> show e
            )
  liftIO $ threadDelay 10000
  writeAndReadLoop

params :: C.Tuple a => C.Consistency -> a -> C.QueryParams a
params c p = C.QueryParams c False p Nothing Nothing Nothing Nothing

createTable :: MonadClient m => m ()
createTable = do
  let q =
        "CREATE COLUMNFAMILY IF NOT EXISTS notifications\
        \( user    uuid\
        \, id      timeuuid\
        \, payload blob\
        \, primary key (user, id)\
        \) with clustering order by (id asc)\
        \   and compaction  = { 'class'               : 'LeveledCompactionStrategy'\
        \                     , 'tombstone_threshold' : 0.1 }\
        \   and compression = { 'sstable_compression' : 'LZ4Compressor' }\
        \   and gc_grace_seconds = 0;" ::
          PrepQuery S () ()
      p = params C.All ()
  void $ C.schema q p

writeAndRead :: MonadClient m => m ()
writeAndRead = do
  userId <- liftIO $ randomUser
  timeId <- liftIO $ fromJust <$> UUIDv1.nextUUID
  let clients = C.Set ["client-1"]
      payload = C.Blob "{\"foo\": 123}"
      writeQuery = "INSERT INTO notifications (user, id, payload, clients) VALUES (?, ?, ?, ?) USING TTL ?" :: PrepQuery W (UUID, UUID, C.Blob, C.Set Text, Int32) ()
      writeParams = params C.LocalQuorum (userId, timeId, payload, clients, 2419200)
  C.write writeQuery writeParams
  let readQuery = "SELECT user, id, payload, clients FROM notifications where user = ? and id > ? limit 100" :: PrepQuery R (UUID, UUID) (UUID, C.TimeUuid, C.Blob, C.Set Text)
      readParams = params C.LocalQuorum (userId, fromJust $ UUID.fromText "13814000-1dd2-11b2-8000-9f9fcbd62c6e") -- UUDv1 For epoch
  readNotifs <- C.query readQuery readParams
  -- liftIO $ putStr "."
  pure ()

randomUser :: MonadRandom m => m UUID
randomUser = do
  i <- getRandomR (0, length users - 1)
  pure (users !! i)

-- Generated using a fair dice.
users :: [UUID]
users =
  mapMaybe
    UUID.fromText
    [ "d833babf-180a-4a71-ac4d-290f32f85b80",
      "41428959-9226-42e6-8f78-0a9417cdd63f",
      "8c3dd94f-5709-4ff6-9119-a5db729696dc",
      "d3b92ef1-fa3f-459d-8cfe-06ffe11747b4",
      "69684691-ca71-446d-a8bb-12a9aca05068",
      "b1c5b4aa-89ee-46cf-a15f-c362d3d46eba",
      "8528e58a-0505-4145-aff3-f364f043e179",
      "aec0168c-7ddf-4049-b634-358fb4b85aa6",
      "cfaf4424-d036-490b-a070-a4fe5ff0972c",
      "b1a8ab1f-3579-427e-ac2f-83bef12ef85c",
      "8346c214-ec92-45a1-81d2-fc02811ea553",
      "e8fd5a2e-edaa-4421-ad1b-c17ecfda0604",
      "22aa4b03-8e2a-4824-bae3-c252bcd0d31c",
      "1594018c-5078-4393-b9d5-faf7e91d694a",
      "0a301171-fce3-4148-96c3-9b16dc08cd73",
      "8ac9fa38-2efe-400d-a7a3-7b82a9f503b9",
      "d6cfb840-4ed0-40ac-9489-741341efd6c6",
      "6b8f3bbe-e921-41d2-b46d-b4e10584e215",
      "9c78d246-3a66-4a56-bdf9-51eff3f712d3",
      "b20c087f-69eb-433a-9acd-f99bc0befd9a",
      "edccb320-3edb-4a7a-8644-723c8c52e279",
      "907c11c9-71a2-484f-bb59-74b7500cd99a",
      "893cdd7e-5261-42c4-8e53-1de4837d4db6",
      "1844610c-d234-4ec2-b737-cd75f3f880e3",
      "ba26f321-b7dd-42f8-bbb2-48e9b222fe40",
      "eb8e548d-e8ed-4941-927c-0e9b98b3acdd",
      "e19e4e61-a926-47d8-a415-6956c25fa782",
      "dd47df77-9770-44fe-9df3-c5041905f3b7",
      "0a0f91b5-bd1b-4bf5-b499-97ff0d6707ee",
      "5da9fe81-7004-4139-ba72-4e955f070f84",
      "d43368c0-85c9-422e-a3f4-d151c5026c32",
      "32231cb2-3af1-4922-a6b4-9252e7cfc7be",
      "1f362b82-f8fb-49fe-bd52-b69c5eb74c87",
      "730ee6b2-5b44-46ee-a702-d3e30169f2a1",
      "f5912a37-7aa7-488f-ac75-f6ff4a84d683",
      "8a5a6bf4-b8ca-4688-8127-7da9c8c1f683",
      "391cf165-eed6-4ac1-a4c0-b737db9cc7f0",
      "20ea8a01-d363-4b90-86d6-e4ce2b052328",
      "77b4d855-fd21-4333-8101-1ced85c28957",
      "62cb5981-91cb-4f4c-b3fc-f6a43d4bd5a0"
    ]
