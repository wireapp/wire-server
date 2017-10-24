{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Gundeck.Aws
    ( -- * Monad
      Env
    , mkEnv
    , Amazon
    , execute

      -- * Errors
    , Error               (..)
    , CreateEndpointError (..)
    , PublishError        (..)

      -- * Endpoints
    , SNSEndpoint
    , endpointToken
    , endpointEnabled
    , endpointUsers

    , createEndpoint
    , deleteEndpoint
    , lookupEndpoint
    , updateEndpoint

      -- * Publish
    , Attributes
    , AWS.Seconds (..)
    , publish
    , timeToLive

      -- * Feedback
    , listen
    ) where

import Blaze.ByteString.Builder (toLazyByteString)
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import Control.Concurrent.Lifted (threadDelay)
import Control.Error hiding (err)
import Control.Exception.Enclosed (handleAny)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Retry (retrying, limitRetries)
import Data.Aeson (decodeStrict)
import Data.Attoparsec.Text
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.Id
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable
import Gundeck.Aws.Arn
import Gundeck.Aws.Sns (Event, evType, evEndpoint)
import Gundeck.Instances ()
import Gundeck.Options
import Gundeck.Types.Push (AppName (..), Transport (..), Token)
import Network.AWS (AWSRequest, Rs)
import Network.AWS (serviceCode, serviceMessage, serviceAbbrev, serviceStatus)
import Network.AWS.SQS (rmrsMessages)
import Network.AWS.SQS.Types
import Network.HTTP.Client (Manager, HttpException (..), HttpExceptionContent (..))
import Network.HTTP.Types
import System.Logger.Class

import qualified Control.Monad.Trans.AWS as AWST
import qualified Data.HashMap.Strict     as Map
import qualified Data.Set                as Set
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Data.Text.Lazy          as LT
import qualified Gundeck.Types.Push      as Push
import qualified Network.AWS             as AWS
import qualified Network.AWS.Env         as AWS
import qualified Network.AWS.Data        as AWS
import qualified Network.AWS.SNS         as SNS
import qualified Network.AWS.SQS         as SQS
import qualified Network.TLS             as TLS
import qualified System.Logger           as Logger

data Error where
    EndpointNotFound :: EndpointArn -> Error
    NoEndpointArn    :: Error
    NoToken          :: EndpointArn -> Error
    InvalidArn       :: Text -> String -> Error
    GeneralError     :: (Show e, AWS.AsError e) => e -> Error

deriving instance Show     Error
deriving instance Typeable Error
instance Exception Error

newtype QueueUrl = QueueUrl Text deriving Show

data Env = Env
    { _awsEnv     :: !AWS.Env
    , _logger     :: !Logger
    , _eventQueue :: !QueueUrl
    , _region     :: !Region
    , _account    :: !Account
    }

data SNSEndpoint = SNSEndpoint
    { _endpointToken   :: !Push.Token
    , _endpointEnabled :: !Bool
    , _endpointUsers   :: !(Set UserId)
    } deriving Show

makeLenses ''Env
makeLenses ''SNSEndpoint

newtype Amazon a = Amazon
    { unAmazon :: ReaderT Env (ResourceT IO) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadBase IO
               , MonadThrow
               , MonadCatch
               , MonadMask
               , MonadReader Env
               , MonadResource
               )

instance MonadLogger Amazon where
    log l m = view logger >>= \g -> Logger.log g l m

instance MonadBaseControl IO Amazon where
    type StM Amazon a = StM (ReaderT Env (ResourceT IO)) a
    liftBaseWith    f = Amazon $ liftBaseWith $ \run -> f (run . unAmazon)
    restoreM          = Amazon . restoreM

instance AWS.MonadAWS Amazon where
    liftAWS a = view awsEnv >>= \e -> AWS.runAWS e a

mkEnv :: Logger -> Opts -> Manager -> IO Env
mkEnv lgr opts mgr = do
    let g = Logger.clone (Just "aws.gundeck") lgr
    e <- configure <$> mkAwsEnv g
    q <- getQueueUrl e (opts^.aws.awsQueueName)
    return (Env e g q (opts^.aws.awsRegion) (opts^.aws.awsAccount))
  where
    mkAwsEnv g =  set AWS.envLogger (awsLogger g)
               .  set AWS.envRegion (opts^.aws.awsRegion)
              <$> AWS.newEnvWith AWS.Discover Nothing mgr

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

    configure = set AWS.envRetryCheck retryCheck
              . AWS.configure snsConfig

    snsConfig = SNS.sns & set AWS.serviceTimeout (Just (AWS.Seconds 5))

    -- Modified version of 'AWS.retryConnectionFailure' to take into
    -- account occasional TLS handshake failures.
    -- See: https://github.com/vincenthz/hs-tls/issues/124
    -- See: https://github.com/brendanhay/amazonka/issues/269
    retryCheck _ InvalidUrlException{} = False
    retryCheck n (HttpExceptionRequest _ ex) = case ex of
        _ | n >= 3                    -> False
        NoResponseDataReceived        -> True
        ConnectionTimeout             -> True
        ConnectionClosed              -> True
        ConnectionFailure _           -> True
        InternalException x           -> case fromException x of
            Just TLS.HandshakeFailed {} -> True
            _                           -> False
        _                             -> False

    getQueueUrl :: AWS.Env -> Text -> IO QueueUrl
    getQueueUrl e q = do
        x <- runResourceT . AWST.runAWST e $
            AWST.trying AWS._Error $
                AWST.send (SQS.getQueueURL q)
        either (throwM . GeneralError)
               (return . QueueUrl . view SQS.gqursQueueURL) x

execute :: MonadIO m => Env -> Amazon a -> m a
execute e m = liftIO $ runResourceT (runReaderT (unAmazon m) e)

--------------------------------------------------------------------------------
-- Endpoints

data CreateEndpointError
    = EndpointInUse !EndpointArn
        -- ^ Endpoint exists with the same token but different attributes.
    | InvalidToken !Push.Token
        -- ^ Invalid push token.
    | AppNotFound !AppName
        -- ^ Invalid application name.
    deriving Show

-- | Update an endpoint with the given push token.
--
-- This will replace the current token, set the endpoint's user data to
-- the the list of given 'UserId's and enable the endpoint.
updateEndpoint :: Set UserId -> Token -> EndpointArn -> Amazon ()
updateEndpoint us tk arn = do
    let req = over SNS.seaAttributes fun (SNS.setEndpointAttributes (toText arn))
    res <- retrying (limitRetries 1) (const isTimeout) (const (sendCatch req))
    case res of
        Right _ -> return ()
        Left  x -> throwM $ if is "SNS" 404 x
                                then EndpointNotFound arn
                                else GeneralError x
  where
    fun = Map.insert "Token" (Push.tokenText tk)
        . Map.insert "CustomUserData" (mkUsers us)
        . Map.insert "Enabled" "true"

    mkUsers = Text.intercalate ":" . map toText . Set.toList

deleteEndpoint :: EndpointArn -> Amazon ()
deleteEndpoint arn = do
    res <- retrying (limitRetries 1) (const isTimeout) (const (sendCatch req))
    either (throwM . GeneralError) (const (return ())) res
  where
    req = SNS.deleteEndpoint (toText arn)

lookupEndpoint :: EndpointArn -> Amazon (Maybe SNSEndpoint)
lookupEndpoint arn = do
    res <- retrying (limitRetries 1) (const isTimeout) (const (sendCatch req))
    let attrs = view SNS.gearsAttributes <$> res
    case attrs of
        Right a -> Just <$> mkEndpoint a
        Left  x -> if is "SNS" 404 x then return Nothing else throwM (GeneralError x)
  where
    req = SNS.getEndpointAttributes (toText arn)

    mkEndpoint a = do
        t <- maybe (throwM $ NoToken arn) return (Map.lookup "Token" a)
        let e = either (const Nothing) Just . fromText =<< Map.lookup "Enabled" a
            d = maybe Set.empty mkUsers $ Map.lookup "CustomUserData" a
        return (SNSEndpoint (Push.Token t) (fromMaybe False e) d)

    mkUsers = Set.fromList . mapMaybe (hush . fromText) . Text.split (== ':')

createEndpoint :: UserId -> Push.Transport -> ArnEnv -> AppName -> Push.Token -> Amazon (Either CreateEndpointError EndpointArn)
createEndpoint u tr env app token = do
    aEnv <- ask
    let top = mkAppTopic env tr app
    let arn = mkSnsArn (aEnv^.region) (aEnv^.account) top
    let tkn = Push.tokenText token
    let req = SNS.createPlatformEndpoint (toText arn) tkn
            & set SNS.cpeCustomUserData (Just (toText u))
            & set SNS.cpeAttributes (Map.insert "Enabled" "true" Map.empty)
    res <- retrying (limitRetries 2) (const isTimeout) (const (sendCatch req))
    case res of
        Right r ->
            case view SNS.cpersEndpointARN r of
                Nothing -> throwM NoEndpointArn
                Just  s -> Right <$> readArn s
        Left x@(AWS.ServiceError e)
            | is "SNS" 400 x && AWS.errorCode "InvalidParameter" == e^.serviceCode
            , Just ep <- parseExistsError (e^.serviceMessage) ->
                return (Left (EndpointInUse ep))
            | is "SNS" 400 x && AWS.errorCode "InvalidParameter" == e^.serviceCode
                             && isTokenError (e^.serviceMessage) ->
                return (Left (InvalidToken token))
            | is "SNS" 404 x ->
                return (Left (AppNotFound app))
            | is "SNS" 403 x -> do
                warn $ "arn" .= toText arn ~~ msg (val "Not authorized.")
                return (Left (AppNotFound app))
        Left x -> throwM (GeneralError x)
  where
    readArn r = either (throwM . InvalidArn r) return (fromText r)

    -- Thank you Amazon for not having granular error codes!
    parseExistsError Nothing  = Nothing
    parseExistsError (Just s) = hush . flip parseOnly (toText s) $ do
        _ <- string "Invalid parameter: Token Reason: Endpoint "
        a <- AWS.parser
        _ <- string " already exists with the same Token, but different attributes."
        return a

    isTokenError Nothing  = False
    isTokenError (Just s) = isRight . flip parseOnly (toText s) $ do
        _ <- string "Invalid parameter: Token"
        return ()

--------------------------------------------------------------------------------
-- Publish

data PublishError
    = EndpointDisabled !EndpointArn
    | InvalidEndpoint  !EndpointArn
    | PayloadTooLarge  !EndpointArn

newtype Attributes = Attributes
    { setAttributes :: Endo (HashMap Text SNS.MessageAttributeValue)
    } deriving Monoid

-- Note [VoIP TTLs]
-- ~~~~~~~~~~~~~~~~
-- The TTL message attributes for APNS_VOIP and APNS_VOIP_SANDBOX are not
-- documented but appear to work. The reason might be that TTLs were
-- introduced before support for VoIP notifications. There is a catch,
-- however. For GCM, APNS and APNS_SANDBOX, SNS treats the TTL "0"
-- specially, i.e. it forwards it to the provider where it has a special
-- meaning. That does not appear to be the case for APNS_VOIP and
-- APNS_VOIP_SANDBOX, for which the TTL is interpreted normally, which means
-- if the TTL is lower than the "dwell time" in SNS, the notification is
-- never sent to the provider. So we must specify a reasonably large TTL
-- for transient VoIP notifications, so that they are not discarded
-- already by SNS.
--
-- cf. http://docs.aws.amazon.com/sns/latest/dg/sns-ttl.html

timeToLive :: Transport -> AWS.Seconds -> Attributes
timeToLive t s = Attributes (Endo (ttlAttr s))
  where
    ttlAttr n | n == 0    = setTTL (ttlNow t)
              | otherwise = setTTL (toText n)

    setTTL v = let ty = SNS.messageAttributeValue "String" in
        Map.insert (ttlKey t) (ty & SNS.mavStringValue .~ Just v)

    ttlNow GCM             = "0"
    ttlNow APNS            = "0"
    ttlNow APNSSandbox     = "0"
    ttlNow APNSVoIP        = "15" -- See note [VoIP TTLs]
    ttlNow APNSVoIPSandbox = "15" -- See note [VoIP TTLs]

    ttlKey GCM             = "AWS.SNS.MOBILE.GCM.TTL"
    ttlKey APNS            = "AWS.SNS.MOBILE.APNS.TTL"
    ttlKey APNSSandbox     = "AWS.SNS.MOBILE.APNS_SANDBOX.TTL"
    ttlKey APNSVoIP        = "AWS.SNS.MOBILE.APNS_VOIP.TTL"
    ttlKey APNSVoIPSandbox = "AWS.SNS.MOBILE.APNS_VOIP_SANDBOX.TTL"

publish :: EndpointArn -> LT.Text -> Attributes -> Amazon (Either PublishError ())
publish arn txt attrs = do
    -- TODO: Make amazonka accept a lazy text or bytestring.
    let req = SNS.publish (LT.toStrict txt)
            & SNS.pTargetARN .~ Just (toText arn)
            & SNS.pMessageStructure .~ Just "json"
            & SNS.pMessageAttributes .~ appEndo (setAttributes attrs) Map.empty
    res <- retrying (limitRetries 3) (const isTimeout) (const (sendCatch req))
    case res of
        Right _ -> return (Right ())
        Left x@(AWS.ServiceError e)
            | is "SNS" 400 x && AWS.errorCode "EndpointDisabled" == e^.serviceCode ->
                return (Left (EndpointDisabled arn))
            | is "SNS" 400 x && AWS.errorCode "InvalidParameter" == e^.serviceCode
                             && isProtocolSizeError (e^.serviceMessage) ->
                return (Left (PayloadTooLarge arn))
            | is "SNS" 400 x && AWS.errorCode "InvalidParameter" == e^.serviceCode
                             && isSnsSizeError (e^.serviceMessage) ->
                return (Left (PayloadTooLarge arn))
            | is "SNS" 400 x && AWS.errorCode "InvalidParameter" == e^.serviceCode
                             && isArnError (e^.serviceMessage) ->
                return (Left (InvalidEndpoint arn))
        Left x -> throwM (GeneralError x)
  where
    -- Thank you Amazon for not having granular error codes!
    -- nb. We don't check the size upfront because it would require serialising
    -- the payload before shipping it to amazonka (as 'Text') just to check the
    -- size, i.e. it would be serialised twice.
    isProtocolSizeError Nothing  = False
    isProtocolSizeError (Just s) = isRight . flip parseOnly (toText s) $ do
        _ <- string "Invalid parameter: Message Reason: Invalid notification for protocol "
        t <- transportParser
        _ <- case t of
            Push.GCM -> string ": Notification data is larger than allowed limit"
            _        -> string ": Notification is too long"
        return ()

    isSnsSizeError Nothing  = False
    isSnsSizeError (Just s) = isRight . flip parseOnly (toText s) $ do
        _ <- string "Invalid parameter: Message too long"
        return ()

    isArnError Nothing  = False
    isArnError (Just s) = isRight . flip parseOnly (toText s) $ do
        _ <- string "Invalid parameter: TargetArn Reason: No endpoint found for the target arn specified"
        return ()

--------------------------------------------------------------------------------
-- Feedback

listen :: (Event -> IO ()) -> Amazon ()
listen callback = do
    QueueUrl url <- view eventQueue
    forever $ handleAny unexpectedError $ do
        msgs <- view rmrsMessages <$> send (receive url)
        void $ mapConcurrently (onMessage url) msgs
  where
    receive url =
        SQS.receiveMessage url
            & set SQS.rmWaitTimeSeconds (Just 20)
            . set SQS.rmMaxNumberOfMessages (Just 10)

    onMessage url m =
        case decodeStrict =<< Text.encodeUtf8 <$> m^.mBody of
            Nothing ->
                err . msg $ val "Failed to parse SQS event notification"
            Just e -> do
                info $ "sqs-event" .= toText (e^.evType)
                    ~~ "arn"       .= toText (e^.evEndpoint)
                    ~~ msg (val "Received SQS event")
                liftIO $ callback e
                for_ (m^.mReceiptHandle) (void . send . SQS.deleteMessage url)

    unexpectedError x = do
        err $ "error" .= show x ~~ msg (val "Failed to read from SQS")
        threadDelay 3000000

--------------------------------------------------------------------------------
-- Utilities

sendCatch :: AWSRequest r => r -> Amazon (Either AWS.Error (Rs r))
sendCatch = AWST.trying AWS._Error . AWS.send

send :: AWSRequest r => r -> Amazon (Rs r)
send r = either (throwM . GeneralError) return =<< sendCatch r

is :: AWS.Abbrev -> Int -> AWS.Error -> Bool
is srv s (AWS.ServiceError e) = srv == e^.serviceAbbrev && s == statusCode (e^.serviceStatus)
is _   _ _                    = False

isTimeout :: MonadIO m => Either AWS.Error a -> m Bool
isTimeout (Right _) = pure False
isTimeout (Left  e) = case e of
    AWS.TransportError (HttpExceptionRequest _ ResponseTimeout) -> pure True
    _                                                           -> pure False

