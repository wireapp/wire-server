{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Util.Test.SQS where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, asyncExceptionFromException)
import Control.Lens hiding ((.=))
import Control.Monad.Catch hiding (bracket)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Data.Either
import Data.Foldable (for_)
import Data.Monoid ((<>))
import Safe (headDef)
import Data.ProtoLens
import Data.Text (Text)
import Test.Tasty.HUnit

import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding     as Text
import qualified Network.AWS            as AWS
import qualified Network.AWS.SQS        as SQS

-----------------------------------------------------------------------------
-- Assertions
assertQueue :: Message a => Text -> String -> AWS.Env -> (String -> Maybe a -> IO ()) -> IO ()
assertQueue url label env check = execute env $ fetchMessage url label check

assertNoMessages :: Text -> AWS.Env -> IO ()
assertNoMessages url env = do
    msgs <- execute env $ readAndDeleteAllUntilEmpty url
    assertEqual "ensureNoMessages: length" 0 (length msgs)

-----------------------------------------------------------------------------
-- Queue operations
purgeQueue :: AWS.MonadAWS m => Text -> m ()
purgeQueue = void . readAndDeleteAllUntilEmpty

-- Note that Amazon's purge queue is a bit incovenient for testing purposes because
-- it may be delayed in ~60 seconds which causes messages that are published later
-- to be (unintentionally) deleted which is why we have our own for testing purposes
readAndDeleteAllUntilEmpty :: AWS.MonadAWS m => Text -> m [SQS.Message]
readAndDeleteAllUntilEmpty url = do
    firstBatch <- view SQS.rmrsMessages <$> AWS.send (receive 1 url)
    allMsgs    <- readUntilEmpty firstBatch firstBatch
    return allMsgs
  where
    readUntilEmpty acc []   = return acc
    readUntilEmpty acc msgs = do
        forM_ msgs $ deleteMessage url
        newMsgs <- view SQS.rmrsMessages <$> AWS.send (receive 1 url)
        forM_ newMsgs $ deleteMessage url
        readUntilEmpty (acc ++ newMsgs) newMsgs

deleteMessage :: AWS.MonadAWS m => Text -> SQS.Message -> m ()
deleteMessage url m = do
    for_ (m ^. SQS.mReceiptHandle)
         (void . AWS.send . SQS.deleteMessage url)

-----------------------------------------------------------------------------
-- Generic AWS execution helpers
execute :: (AWS.HasEnv r, MonadIO m, MonadThrow m, MonadBaseControl IO m)
           => r -> AWS.AWS a -> m a
execute env act = liftIO . AWS.runResourceT $ AWS.runAWS env act

-----------------------------------------------------------------------------
-- Internal. Most of these functions _can_ be used outside of this function
-- but probably do not need to
receive :: Int -> Text -> SQS.ReceiveMessage
receive n url = SQS.receiveMessage url
              & set SQS.rmWaitTimeSeconds (Just 1)
              . set SQS.rmMaxNumberOfMessages (Just n)
              . set SQS.rmVisibilityTimeout (Just 1)

fetchMessage :: (MonadIO m, AWS.MonadAWS m, Message a) => Text -> String -> (String -> Maybe a -> IO ()) -> m ()
fetchMessage url label callback = do
    msgs <- view SQS.rmrsMessages <$> AWS.send (receive 1 url)
    events <- mapM (parseDeleteMessage url) msgs
    liftIO $ callback label (headDef Nothing events)

parseDeleteMessage :: (AWS.MonadAWS m, Message a) => Text -> SQS.Message -> m (Maybe a)
parseDeleteMessage url m = do
    evt <- case (>>= decodeMessage) . B64.decode . Text.encodeUtf8 <$> (m^.SQS.mBody) of
        Just (Right e) -> return (Just e)
        _              -> do
            liftIO $ print ("Failed to parse SQS message or event" :: String)
            return Nothing
    deleteMessage url m
    return evt

queueMessage :: (AWS.MonadAWS m, Message a) => Text -> a -> m ()
queueMessage url e = void $ AWS.send req
  where
    event = Text.decodeLatin1 $ B64.encode $ encodeMessage e
    req   = SQS.sendMessage url event

newtype MatchFailure a = MatchFailure { mFailure :: (a, SomeException) }

-- Try to match some assertions (callback) during the given timeout; if there's no
-- match during the timeout, it asserts with the given label
-- Matched matches are consumed while unmatched ones are republished to the queue
tryMatch :: (AWS.MonadAWS m, Show a, Message a)
         => String
         -> Int
         -> Text
         -> (String -> Maybe a -> IO())
         -> m ()
tryMatch label tries url callback = go tries
  where
    go 0 = liftIO (assertFailure $ label <> ": No matching event found")
    go n = do
        msgs      <- readAndDeleteAllUntilEmpty url
        (bad, ok) <- partitionEithers <$> mapM (check <=< parseDeleteMessage url) msgs
        -- Requeue all failed checks
        forM_ bad $ \x -> for_ (fst . mFailure $ x) (queueMessage url)
        -- If no success, continue!
        when (null ok) $ do
            liftIO $ threadDelay (10^(6 :: Int))
            go (n - 1)

    check e = do
        liftIO $ callback label e
        return (Right $ show e)
      `catchAll` \ex -> case asyncExceptionFromException ex of
        Just  x -> throwM (x :: SomeAsyncException)
        Nothing -> return . Left $ MatchFailure (e, ex)
