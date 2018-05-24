{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Journal where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Retry
import Data.Id
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (fromStrict)
import Data.Proto
import Data.Proto.Id
import Data.Text (Text)
import Data.UUID
import Brig.Types
import Data.Typeable
import Data.Monoid ((<>))
import Data.Int
import Data.Maybe
import Data.ProtoLens.Encoding (encodeMessage)
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Client (Manager)
import Proto.UserEvents hiding (userId)
import Network.AWS (AWSRequest, Rs)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import System.Logger (Logger)
import Util.Options

import qualified Brig.AWS                as BrigAWS
import qualified Cassandra               as C
import qualified Control.Monad.Trans.AWS as AWST
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text.Encoding      as Text
import qualified Data.ByteString.Base64  as B64
import qualified System.Logger           as Logger
import qualified Network.AWS             as AWS
import qualified Network.AWS.Env         as AWS
import qualified Network.AWS.SQS         as SQS

data AWSEnv = AWSEnv
    { _logger           :: !Logger
    , _userJournalQueue :: !Text
    , _amazonkaEnv      :: !AWS.Env
    }

makeLenses ''AWSEnv

runCommand :: Logger -> AWSEnv -> C.ClientState -> Maybe UserId -> IO ()
runCommand l env c start = void $ C.runClient c $ do
    page <- case start of
        Just st -> C.retry C.x5 $ C.paginate userSelectFrom (C.paramsP C.Quorum (Identity st) 100)
        Nothing -> C.retry C.x5 $ C.paginate userSelect (C.paramsP C.Quorum () 100)
    scan 0 0 page
  where
    scan :: Int -> Int -> C.Page UserDB -> C.Client ()
    scan acc emailAcc page = do
        let res   = C.result page
        let tCount = acc + Prelude.length res
        let active = mapMaybe toActiveUserWithEmail res
        let activeEmailCount = emailAcc + Prelude.length active

        void $ error "Execute action here" -- journal ...

        when (C.hasMore page) $
            C.retry C.x5 (C.liftClient (C.nextPage page)) >>= scan tCount activeEmailCount

    -- 0 is status == ACTIVE
    toActiveUserWithEmail :: UserDB -> Maybe ActiveUserWithEmail
    toActiveUserWithEmail (uid, Just e, Just True, Just 0, t) = Just (uid, e, isJust t)
    toActiveUserWithEmail  _                                  = Nothing

    journal :: (UserId, Email) -> IO ()
    journal (u, e) = do
        ts  <- now
        rnd <- liftIO nextRandom
        let encoded = fromStrict . B64.encode . encodeMessage
                    $ UserEvent UserEvent'USER_ACTIVATE (toBytes u) ts (Just $ toByteString' e) Nothing
                    -- ^ TODO: should we use the locale?  (pack . show <$> loc)
        void $ executeAWS env (enqueueFIFO "staging-user-events.fifo" "user.events" rnd encoded)

-- CQL queries
userSelect :: C.PrepQuery C.R () UserDB
userSelect = "SELECT id, email, activated, status, team FROM user"

userSelectFrom :: C.PrepQuery C.R (Identity UserId) UserDB
userSelectFrom = "SELECT id, email, activated, status, team FROM user WHERE token(id) > token(?)"

-- Utils

type UserDB              = (UserId, Maybe Text, Maybe Bool, Maybe Int32, Maybe TeamId)
type ActiveUserWithEmail = (UserId, Text, Bool) -- email, is_team_user

-- Utilities
newtype Amazon a = Amazon
    { unAmazon :: ReaderT AWSEnv (ResourceT IO) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadBase IO
               , MonadThrow
               , MonadCatch
               , MonadMask
               , MonadReader AWSEnv
               , MonadResource
               )

instance AWS.MonadAWS Amazon where
    liftAWS a = view amazonkaEnv >>= flip AWS.runAWS a

mkEnv :: Logger -> Text -> Manager -> IO AWSEnv
mkEnv lgr journalQueueName mgr = do
    let g = Logger.clone (Just "aws.brig-journaler") lgr
    e  <- mkAwsEnv g $ mkEndpoint SQS.sqs (fromMaybe (error "Failed to parse") $ fromByteString' "https://sqs.eu-west-1.amazonaws.com")
    jq <- getQueueUrl e journalQueueName
    -- TODO: Use the QueueURL
    return (AWSEnv g journalQueueName e)
  where
    mkEndpoint svc e = AWS.setEndpoint (e^.awsSecure) (e^.awsHost) (e^.awsPort) svc

    mkAwsEnv g sqs =  set AWS.envLogger (awsLogger g)
                  <$> AWS.newEnvWith AWS.Discover Nothing mgr
                  <&> AWS.configure sqs

    awsLogger g l = Logger.log g (mapLevel l) . Logger.msg . toLazyByteString

    mapLevel AWS.Info  = Logger.Info
    -- Debug output from amazonka can be very useful for tracing requests
    -- but is very verbose (and multiline which we don't handle well)
    -- distracting from our own debug logs, so we map amazonka's 'Debug'
    -- level to our 'Trace' level.
    mapLevel AWS.Debug = Logger.Trace
    mapLevel AWS.Trace = Logger.Trace
    -- n.b. Errors are either returned or thrown. In both cases they will
    -- already be logged if left unhandled. We don't want errors to be
    -- logged inside amazonka already, before we even had a chance to handle
    -- them, which results in distracting noise. For debugging purposes,
    -- they are still revealed on debug level.
    mapLevel AWS.Error = Logger.Debug

    getQueueUrl e q = view SQS.gqursQueueURL <$> BrigAWS.exec e (SQS.getQueueURL q)

data Error where
    GeneralError     :: (Show e, AWS.AsError e) => e -> Error
    SESInvalidDomain :: Error

deriving instance Show     Error
deriving instance Typeable Error

instance Exception Error

executeAWS :: MonadIO m => AWSEnv -> Amazon a -> m a
executeAWS e m = liftIO $ runResourceT (runReaderT (unAmazon m) e)

enqueueFIFO :: Text -> Text -> UUID -> BL.ByteString -> Amazon (SQS.SendMessageResponse)
enqueueFIFO url group dedup m = retrying retry5x (const canRetry) (const (sendCatch req)) >>= throwA
  where
    req = SQS.sendMessage url ( Text.decodeLatin1 (BL.toStrict m))
                              & SQS.smMessageGroupId .~ Just group
                              & SQS.smMessageDeduplicationId .~ Just (toText dedup)

sendCatch :: AWSRequest r => r -> Amazon (Either AWS.Error (Rs r))
sendCatch = AWST.trying AWS._Error . AWS.send

send :: AWSRequest r => r -> Amazon (Rs r)
send r = throwA =<< sendCatch r

throwA :: Either AWS.Error a -> Amazon a
throwA = either (throwM . GeneralError) return

execCatch :: (AWSRequest a, AWS.HasEnv r, MonadCatch m, MonadThrow m, MonadBaseControl IO m, MonadBase IO m, MonadIO m)
          => r -> a -> m (Either AWS.Error (Rs a))
execCatch e cmd = runResourceT . AWST.runAWST e
                $ AWST.trying AWS._Error $ AWST.send cmd

exec :: (AWSRequest a, AWS.HasEnv r, MonadCatch m, MonadThrow m, MonadBaseControl IO m, MonadBase IO m, MonadIO m)
     => r -> a -> m (Rs a)
exec e cmd = execCatch e cmd >>= either (throwM . GeneralError) return

canRetry :: MonadIO m => Either AWS.Error a -> m Bool
canRetry (Right _) = pure False
canRetry (Left  e) = case e of
    AWS.TransportError (HttpExceptionRequest _ ResponseTimeout)                   -> pure True
    AWS.ServiceError se | se^.AWS.serviceCode == AWS.ErrorCode "RequestThrottled" -> pure True
    _                                                                             -> pure False

retry5x :: (Monad m) => RetryPolicyM m
retry5x = limitRetries 5 <> exponentialBackoff 100000
