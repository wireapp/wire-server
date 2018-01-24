{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module API.SQS where

import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Foldable (for_)
import Data.Id
import Data.Int
import Data.Monoid ((<>))
import Data.Text (pack)
import Safe (headDef)
import Data.ByteString.Lazy (toStrict)
import Data.ProtoLens.Encoding
import Data.Text (Text)
import Galley.Aws
import Galley.Options (JournalOpts(..), FakeSQSOpts(..))
import Network.HTTP.Client
import Network.HTTP.Client.OpenSSL
import OpenSSL.Session as Ssl
import System.Logger.Class
import Test.Tasty.HUnit

import qualified Data.ByteString.Base64 as B64
import qualified Data.Currency as Currency
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import qualified Galley.Aws as Aws
import qualified Network.AWS as AWS
import qualified Network.AWS.SQS as SQS
import qualified OpenSSL.X509.SystemStore as Ssl
import qualified Proto.TeamEvents as E
import qualified System.Logger as L

assertQueue :: MonadIO m => String -> Maybe Aws.Env -> (String -> Maybe E.TeamEvent -> IO ()) -> m ()
assertQueue label (Just env) check = liftIO $ Aws.execute env $ fetchMessage label check
assertQueue _ Nothing _ = return ()

assertQueueEmpty :: MonadIO m => Maybe Aws.Env -> m ()
assertQueueEmpty (Just env) = liftIO $ Aws.execute env ensureNoMessages
assertQueueEmpty Nothing = return ()

tActivateWithCurrency :: Maybe Currency.Alpha -> String -> Maybe E.TeamEvent -> IO ()
tActivateWithCurrency c l (Just e) = do
    assertEqual (l <> ": eventType") E.TeamEvent'TEAM_ACTIVATE (e^.E.eventType)
    assertEqual "count" 1 (e^.E.eventData^.E.memberCount)
    -- NOTE: protobuf decodes absent, optional fields as (Just "")
    let cur = maybe "" (pack . show) c
    assertEqual "currency" (Just cur) (e^.E.eventData^?E.currency)
tActivateWithCurrency _ l Nothing = assertFailure $ l <> ": Expected 1 TeamActivate, got nothing"

tActivate :: String -> Maybe E.TeamEvent -> IO ()
tActivate l (Just e) = do
    assertEqual (l <> ": eventType") E.TeamEvent'TEAM_ACTIVATE (e^.E.eventType)
    assertEqual "count" 1 (e^.E.eventData^.E.memberCount)
tActivate l Nothing = assertFailure $ l <> ": Expected 1 TeamActivate, got nothing"

tDelete :: String -> Maybe E.TeamEvent -> IO ()
tDelete l (Just e) = assertEqual (l <> ": eventType") E.TeamEvent'TEAM_DELETE (e^.E.eventType)
tDelete l Nothing = assertFailure $ l <> ": Expected 1 TeamDelete, got nothing"

tSuspend :: String -> Maybe E.TeamEvent -> IO ()
tSuspend l (Just e) = assertEqual (l  <> "eventType") E.TeamEvent'TEAM_SUSPEND (e^.E.eventType)
tSuspend l Nothing = assertFailure $ l <> ": Expected 1 TeamSuspend, got nothing"

tUpdate :: Int32 -> [UserId] -> String -> Maybe E.TeamEvent -> IO ()
tUpdate c uids l (Just e) = do
    assertEqual (l <> "eventType") E.TeamEvent'TEAM_UPDATE (e^.E.eventType)
    assertEqual "count" c (e^.E.eventData^.E.memberCount)
    assertEqual "billing users" (toStrict . UUID.toByteString . toUUID <$> uids) (e^.E.eventData^.E.billingUser)
tUpdate _ _ l Nothing = assertFailure $ l <> ": Expected 1 TeamUpdate, got nothing"

ensureNoMessages :: Amazon ()
ensureNoMessages = do
    QueueUrl url <- view eventQueue
    msgs <- view SQS.rmrsMessages <$> AWS.send (receive url)
    liftIO $ assertEqual "length" 0 (length msgs)

fetchMessage :: String -> (String -> Maybe E.TeamEvent -> IO()) -> Amazon ()
fetchMessage label callback = do
    QueueUrl url <- view eventQueue
    msgs <- view SQS.rmrsMessages <$> AWS.send (receive url)
    events <- mapM (parseDeleteMessage url) msgs
    liftIO $ callback label (headDef Nothing events)

purgeQueue :: Amazon ()
purgeQueue = do
    QueueUrl url <- view eventQueue
    void $ AWS.send (SQS.purgeQueue url)

receive :: Text -> SQS.ReceiveMessage
receive url = SQS.receiveMessage url
                & set SQS.rmWaitTimeSeconds (Just 5)
                . set SQS.rmMaxNumberOfMessages (Just 1)

parseDeleteMessage :: Text -> SQS.Message -> Amazon (Maybe E.TeamEvent)
parseDeleteMessage url m =
  case (>>= decodeMessage) . B64.decode . Text.encodeUtf8 <$> (m^.SQS.mBody) of
      Just (Right e) -> do
          trace $ msg $ val "SQS event received"
          for_ (m ^. SQS.mReceiptHandle) (void . AWS.send . SQS.deleteMessage url)
          return (Just e)
      _ -> do
          err . msg $ val "Failed to parse SQS event"
          return Nothing

initHttpManager :: IO Manager
initHttpManager = do
    ctx <- Ssl.context
    Ssl.contextSetVerificationMode ctx $ Ssl.VerifyPeer True True Nothing
    Ssl.contextAddOption ctx SSL_OP_NO_SSLv2
    Ssl.contextAddOption ctx SSL_OP_NO_SSLv3
    Ssl.contextAddOption ctx SSL_OP_NO_TLSv1
    Ssl.contextSetCiphers ctx rsaCiphers
    Ssl.contextLoadSystemCerts ctx
    newManager (opensslManagerSettings ctx)
        { managerResponseTimeout     = responseTimeoutMicro 10000000
        , managerConnCount           = 100
        , managerIdleConnectionCount = 300
        }

mkAWSEnv :: Maybe FakeSQSOpts -> Text -> IO Aws.Env
mkAWSEnv sqs queue = do
    l   <- L.new $ L.setOutput L.StdOut . L.setFormat Nothing $ L.defSettings
    mgr <- initHttpManager
    let opts = JournalOpts queue AWS.Ireland sqs
    Aws.mkEnv l mgr opts
