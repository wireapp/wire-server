{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Gundeck.Aws
  ( -- * Monad
    Env (..),
    mkEnv,
    Amazon,
    execute,
    Gundeck.Aws.region,
    Gundeck.Aws.account,

    -- * Errors
    Error (..),
    CreateEndpointError (..),
    PublishError (..),

    -- * Endpoints
    SNSEndpoint,
    endpointToken,
    endpointEnabled,
    endpointUsers,
    createEndpoint,
    deleteEndpoint,
    lookupEndpoint,
    updateEndpoint,

    -- * Publish
    Attributes,
    AWS.Seconds (..),
    publish,
    timeToLive,

    -- * Feedback
    listen,

    -- * Exported for testing purposes
    parseExistsError,
  )
where

import Amazonka (AWSRequest, AWSResponse, serviceError_abbrev, serviceError_code, serviceError_message, serviceError_status)
import Amazonka qualified as AWS
import Amazonka.Data.Text qualified as AWS
import Amazonka.SNS qualified as SNS
import Amazonka.SNS.Lens qualified as SNS
import Amazonka.SQS qualified as SQS
import Amazonka.SQS.Lens qualified as SQS
import Amazonka.SQS.Types
import Control.Category ((>>>))
import Control.Error hiding (err, isRight)
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Control.Retry
import Data.Aeson (decodeStrict)
import Data.Attoparsec.Text
import Data.ByteString.Builder
import Data.HashMap.Strict qualified as Map
import Data.Id
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LT
import Gundeck.Aws.Arn
import Gundeck.Aws.Sns
import Gundeck.Instances ()
import Gundeck.Options (Opts)
import Gundeck.Options qualified as O
import Gundeck.Types.Push hiding (token)
import Gundeck.Types.Push qualified as Push
import Imports
import Network.HTTP.Client
import Network.HTTP.Types
import Network.TLS qualified as TLS
import System.Logger qualified as Logger
import System.Logger.Class
import UnliftIO.Async
import UnliftIO.Exception
import Util.Options

data Error where
  EndpointNotFound :: EndpointArn -> Error
  InvalidCustomData :: EndpointArn -> Error
  NoEndpointArn :: Error
  NoToken :: EndpointArn -> Error
  InvalidArn :: Text -> String -> Error
  GeneralError :: (Show e, AWS.AsError e) => e -> Error

deriving instance Show Error

deriving instance Typeable Error

instance Exception Error

newtype QueueUrl = QueueUrl Text deriving (Show)

data Env = Env
  { _awsEnv :: !AWS.Env,
    _logger :: !Logger,
    _eventQueue :: !QueueUrl,
    _region :: !Region,
    _account :: !Account
  }

data SNSEndpoint = SNSEndpoint
  { _endpointToken :: !Push.Token,
    _endpointEnabled :: !Bool,
    _endpointUsers :: !(Set UserId)
  }
  deriving (Show)

makeLenses ''Env

makeLenses ''SNSEndpoint

newtype Amazon a = Amazon
  { unAmazon :: ReaderT Env (ResourceT IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader Env,
      MonadResource,
      MonadUnliftIO
    )

instance MonadLogger Amazon where
  log l m = view logger >>= \g -> Logger.log g l m

mkEnv :: Logger -> Opts -> Manager -> IO Env
mkEnv lgr opts mgr = do
  let g = Logger.clone (Just "aws.gundeck") lgr
  e <-
    mkAwsEnv
      g
      (mkEndpoint SQS.defaultService (opts ^. O.aws . O.sqsEndpoint))
      (mkEndpoint SNS.defaultService (opts ^. O.aws . O.snsEndpoint))
  q <- getQueueUrl e (opts ^. O.aws . O.queueName)
  pure (Env e g q (opts ^. O.aws . O.region) (opts ^. O.aws . O.account))
  where
    mkEndpoint svc e = AWS.setEndpoint (e ^. awsSecure) (e ^. awsHost) (e ^. awsPort) svc
    mkAwsEnv g sqs sns = do
      baseEnv <-
        AWS.newEnv AWS.discover
          <&> do
            AWS.configureService sqs
              >>> AWS.configureService (sns & set AWS.service_timeout (Just (AWS.Seconds 5)))
      pure $
        baseEnv
          { AWS.logger = awsLogger g,
            AWS.region = opts ^. O.aws . O.region,
            AWS.retryCheck = retryCheck,
            AWS.manager = mgr
          }

    awsLogger g l = Logger.log g (mapLevel l) . Logger.msg . toLazyByteString
    mapLevel AWS.Info = Logger.Info
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
    -- Modified version of 'AWS.retryConnectionFailure' to take into
    -- account occasional TLS handshake failures.
    -- See: https://github.com/vincenthz/hs-tls/issues/124
    -- See: https://github.com/brendanhay/amazonka/issues/269
    retryCheck _ InvalidUrlException {} = False
    retryCheck n (HttpExceptionRequest _ ex) = case ex of
      _ | n >= 3 -> False
      NoResponseDataReceived -> True
      ConnectionTimeout -> True
      ConnectionClosed -> True
      ConnectionFailure _ -> True
      InternalException x -> case fromException x of
        Just TLS.HandshakeFailed {} -> True
        _ -> False
      _ -> False
    getQueueUrl :: AWS.Env -> Text -> IO QueueUrl
    getQueueUrl e q = do
      x <-
        runResourceT $
          AWS.trying AWS._Error $
            AWS.send e (SQS.newGetQueueUrl q)
      either
        (throwM . GeneralError)
        (pure . QueueUrl . view SQS.getQueueUrlResponse_queueUrl)
        x

execute :: MonadIO m => Env -> Amazon a -> m a
execute e m = liftIO $ runResourceT (runReaderT (unAmazon m) e)

--------------------------------------------------------------------------------
-- Endpoints

data CreateEndpointError
  = -- | Endpoint exists with the same token but different attributes.
    EndpointInUse !EndpointArn
  | -- | Invalid push token.
    InvalidToken !Push.Token
  | -- | Token is length is greater than 8192 for GCM, or 400 for APNS
    TokenTooLong !Integer
  | -- | Invalid application name.
    AppNotFound !AppName
  deriving (Show)

-- | Update an endpoint with the given push token.
--
-- This will replace the current token, set the endpoint's user data to
-- the the list of given 'UserId's and enable the endpoint.
updateEndpoint :: Set UserId -> Token -> EndpointArn -> Amazon ()
updateEndpoint us tk arn = do
  let req = over SNS.setEndpointAttributes_attributes fun (SNS.newSetEndpointAttributes (toText arn))
  env <- ask
  res <- retry 1 (const (sendCatch (env ^. awsEnv) req))
  case res of
    Right _ -> pure ()
    Left x@(AWS.ServiceError e)
      | is "SNS" 400 x
          && AWS.newErrorCode "InvalidParameter" == e ^. serviceError_code
          && isMetadataLengthError (e ^. serviceError_message) ->
          throwM $ InvalidCustomData arn
    Left x ->
      throwM $
        if is "SNS" 404 x
          then EndpointNotFound arn
          else GeneralError x
  where
    fun =
      Map.insert "Token" (Push.tokenText tk)
        . Map.insert "CustomUserData" (mkUsers us)
        . Map.insert "Enabled" "true"
    mkUsers = Text.intercalate ":" . map toText . Set.toList
    isMetadataLengthError Nothing = False
    isMetadataLengthError (Just s) = isRight . flip parseOnly (toText s) $ do
      let prefix = "Invalid parameter: Attributes Reason: "
      _ <- string prefix
      _ <- string "Invalid value for attribute: CustomUserData: must be at most 2048 bytes long in UTF-8 encoding"
      pure ()

deleteEndpoint :: EndpointArn -> Amazon ()
deleteEndpoint arn = do
  e <- view awsEnv
  res <- retry 1 (const (sendCatch e req))
  either (throwM . GeneralError) (const (pure ())) res
  where
    req = SNS.newDeleteEndpoint (toText arn)

lookupEndpoint :: EndpointArn -> Amazon (Maybe SNSEndpoint)
lookupEndpoint arn = do
  e <- view awsEnv
  res <- retry 1 (const (sendCatch e req))
  let attrs = fromMaybe mempty . view SNS.getEndpointAttributesResponse_attributes <$> res
  case attrs of
    Right a -> Just <$> mkEndpoint a
    Left x -> if is "SNS" 404 x then pure Nothing else throwM (GeneralError x)
  where
    req = SNS.newGetEndpointAttributes (toText arn)
    mkEndpoint a = do
      t <- maybe (throwM $ NoToken arn) pure (Map.lookup "Token" a)
      let e = either (const Nothing) Just . fromText =<< Map.lookup "Enabled" a
          d = maybe Set.empty mkUsers $ Map.lookup "CustomUserData" a
      pure (SNSEndpoint (Push.Token t) (fromMaybe False e) d)
    mkUsers = Set.fromList . mapMaybe (hush . fromText) . Text.split (== ':')

createEndpoint :: UserId -> Push.Transport -> ArnEnv -> AppName -> Push.Token -> Amazon (Either CreateEndpointError EndpointArn)
createEndpoint u tr arnEnv app token = do
  env <- ask
  let top = mkAppTopic arnEnv tr app
  let arn = mkSnsArn env._region env._account top
  let tkn = Push.tokenText token
  let req =
        SNS.newCreatePlatformEndpoint (toText arn) tkn
          & set SNS.createPlatformEndpoint_customUserData (Just (toText u))
          & set SNS.createPlatformEndpoint_attributes (Just $ Map.insert "Enabled" "true" Map.empty)
  res <- retry 2 (const (sendCatch (env ^. awsEnv) req))
  case res of
    Right r ->
      case view SNS.createPlatformEndpointResponse_endpointArn r of
        Nothing -> throwM NoEndpointArn
        Just s -> Right <$> readArn s
    Left x@(AWS.ServiceError e)
      | is "SNS" 400 x && AWS.newErrorCode "InvalidParameter" == e ^. serviceError_code,
        Just ep <- parseExistsError (e ^. serviceError_message) ->
          pure (Left (EndpointInUse ep))
      | is "SNS" 400 x
          && AWS.newErrorCode "InvalidParameter" == e ^. serviceError_code
          && isLengthError (e ^. serviceError_message) ->
          pure (Left (TokenTooLong $ tokenLength token))
      | is "SNS" 400 x
          && AWS.newErrorCode "InvalidParameter" == e ^. serviceError_code
          && isTokenError (e ^. serviceError_message) -> do
          debug $
            msg @Text "InvalidParameter: InvalidToken"
              . field "response" (show x)
          pure (Left (InvalidToken token))
      | is "SNS" 404 x ->
          pure (Left (AppNotFound app))
      | is "SNS" 403 x -> do
          warn $ "arn" .= toText arn ~~ msg (val "Not authorized.")
          pure (Left (AppNotFound app))
    Left x -> throwM (GeneralError x)
  where
    readArn r = either (throwM . InvalidArn r) pure (fromText r)
    tokenLength = toInteger . Text.length . Push.tokenText
    isTokenError Nothing = False
    isTokenError (Just s) = isRight . flip parseOnly (toText s) $ do
      _ <- string "Invalid parameter: Token"
      pure ()
    isLengthError Nothing = False
    isLengthError (Just s) = isRight . flip parseOnly (toText s) $ do
      let prefix = "Invalid parameter: Token Reason: "
      _ <- string prefix
      _ <-
        string "must be at most 8192 bytes long in UTF-8 encoding"
          <|> string "iOS device tokens must be no more than 400 hexadecimal characters"
      pure ()

-- Thank you Amazon for not having granular error codes!
parseExistsError :: Maybe AWS.ErrorMessage -> Maybe EndpointArn
parseExistsError Nothing = Nothing
parseExistsError (Just s) = hush . flip parseOnly (toText s) $ do
  _ <- string "Invalid parameter: Token Reason: Endpoint "
  let endParser = string " already exists with the same Token, but different attributes."
  manyTill anyChar endParser >>= either fail pure . AWS.fromText . Text.pack

--------------------------------------------------------------------------------
-- Publish

data PublishError
  = EndpointDisabled !EndpointArn
  | InvalidEndpoint !EndpointArn
  | PayloadTooLarge !EndpointArn
  | UnauthorisedEndpoint !EndpointArn

newtype Attributes = Attributes
  { setAttributes :: Endo (HashMap Text SNS.MessageAttributeValue)
  }
  deriving (Semigroup, Monoid)

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
    ttlAttr n
      | n == 0 = setTTL (ttlNow t)
      | otherwise = setTTL (toText n)
    setTTL v =
      let ty = SNS.newMessageAttributeValue "String"
       in Map.insert (ttlKey t) (ty & SNS.messageAttributeValue_stringValue ?~ v)
    ttlNow GCM = "0"
    ttlNow APNS = "0"
    ttlNow APNSSandbox = "0"
    ttlNow APNSVoIP = "15" -- See note [VoIP TTLs]
    ttlNow APNSVoIPSandbox = "15" -- See note [VoIP TTLs]
    ttlKey GCM = "AWS.SNS.MOBILE.GCM.TTL"
    ttlKey APNS = "AWS.SNS.MOBILE.APNS.TTL"
    ttlKey APNSSandbox = "AWS.SNS.MOBILE.APNS_SANDBOX.TTL"
    ttlKey APNSVoIP = "AWS.SNS.MOBILE.APNS_VOIP.TTL"
    ttlKey APNSVoIPSandbox = "AWS.SNS.MOBILE.APNS_VOIP_SANDBOX.TTL"

publish :: EndpointArn -> LT.Text -> Attributes -> Amazon (Either PublishError ())
publish arn txt attrs = do
  -- TODO: Make amazonka accept a lazy text or bytestring.
  let req =
        SNS.newPublish (LT.toStrict txt)
          & SNS.publish_targetArn ?~ toText arn
          & SNS.publish_messageStructure ?~ "json"
          & SNS.publish_messageAttributes ?~ appEndo (setAttributes attrs) Map.empty
  env <- ask
  res <- retry 3 (const (sendCatch (env ^. awsEnv) req))
  case res of
    Right _ -> pure (Right ())
    Left x@(AWS.ServiceError e)
      | is "SNS" 400 x && AWS.newErrorCode "EndpointDisabled" == e ^. serviceError_code ->
          pure (Left (EndpointDisabled arn))
      | is "SNS" 400 x
          && AWS.newErrorCode "InvalidParameter" == e ^. serviceError_code
          && isProtocolSizeError (e ^. serviceError_message) ->
          pure (Left (PayloadTooLarge arn))
      | is "SNS" 400 x
          && AWS.newErrorCode "InvalidParameter" == e ^. serviceError_code
          && isSnsSizeError (e ^. serviceError_message) ->
          pure (Left (PayloadTooLarge arn))
      | is "SNS" 400 x
          && AWS.newErrorCode "InvalidParameter" == e ^. serviceError_code
          && isArnError (e ^. serviceError_message) ->
          pure (Left (InvalidEndpoint arn))
      | is "SNS" 403 x
          && AWS.newErrorCode "AuthorizationError" == e ^. serviceError_code ->
          pure (Left (UnauthorisedEndpoint arn))
    Left x -> throwM (GeneralError x)
  where
    -- Thank you Amazon for not having granular error codes!
    -- nb. We don't check the size upfront because it would require serialising
    -- the payload before shipping it to amazonka (as 'Text') just to check the
    -- size, i.e. it would be serialised twice.
    isProtocolSizeError Nothing = False
    isProtocolSizeError (Just s) = isRight . flip parseOnly (toText s) $ do
      _ <- string "Invalid parameter: Message Reason: Invalid notification for protocol "
      t <- transportParser
      _ <- case t of
        Push.GCM -> string ": Notification data is larger than allowed limit"
        _ -> string ": Notification is too long"
      pure ()
    isSnsSizeError Nothing = False
    isSnsSizeError (Just s) = isRight . flip parseOnly (toText s) $ do
      _ <- string "Invalid parameter: Message too long"
      pure ()
    isArnError Nothing = False
    isArnError (Just s) = isRight . flip parseOnly (toText s) $ do
      _ <- string "Invalid parameter: TargetArn Reason: No endpoint found for the target arn specified"
      pure ()

--------------------------------------------------------------------------------
-- Feedback

listen :: Int -> (Event -> IO ()) -> Amazon ()
listen throttleMillis callback = do
  amazonkaEnv <- view awsEnv
  QueueUrl url <- view eventQueue
  forever . handleAny unexpectedError $ do
    msgs <- fromMaybe [] . view SQS.receiveMessageResponse_messages <$> send amazonkaEnv (receive url)
    void $ mapConcurrently (onMessage amazonkaEnv url) msgs
    when (null msgs) $
      threadDelay (1000 * throttleMillis)
  where
    receive url =
      SQS.newReceiveMessage url
        & set SQS.receiveMessage_waitTimeSeconds (Just 20)
          . set SQS.receiveMessage_maxNumberOfMessages (Just 10)
    onMessage awsE url m =
      case decodeStrict . Text.encodeUtf8 =<< (m ^. SQS.message_body) of
        Nothing ->
          err . msg $ val "Failed to parse SQS event notification"
        Just e -> do
          info $
            "sqs-event" .= toText (e ^. evType)
              ~~ "arn" .= toText (e ^. evEndpoint)
              ~~ msg (val "Received SQS event")
          liftIO $ callback e
          for_ (m ^. message_receiptHandle) (void . send awsE . SQS.newDeleteMessage url)
    unexpectedError x = do
      err $ "error" .= show x ~~ msg (val "Failed to read from SQS")
      threadDelay 3000000

--------------------------------------------------------------------------------
-- Utilities

sendCatch ::
  (AWSRequest r, Typeable r, Typeable (AWSResponse r)) =>
  AWS.Env ->
  r ->
  Amazon (Either AWS.Error (AWSResponse r))
sendCatch env = AWS.trying AWS._Error . AWS.send env

send ::
  (AWSRequest r, Typeable r, Typeable (AWSResponse r)) =>
  AWS.Env ->
  r ->
  Amazon (AWSResponse r)
send env r = either (throwM . GeneralError) pure =<< sendCatch env r

is :: AWS.Abbrev -> Int -> AWS.Error -> Bool
is srv s (AWS.ServiceError e) = srv == e ^. serviceError_abbrev && s == statusCode (e ^. serviceError_status)
is _ _ _ = False

retry :: Int -> (RetryStatus -> Amazon (Either AWS.Error a)) -> Amazon (Either AWS.Error a)
retry n =
  retrying
    (exponentialBackoff 50000 <> limitRetries n)
    (const $ \x -> pure $ isTimeout x || isRateLimited x)
  where
    isTimeout :: Either AWS.Error a -> Bool
    isTimeout (Left (AWS.TransportError (HttpExceptionRequest _ ResponseTimeout))) = True
    isTimeout _ = False
    isRateLimited :: Either AWS.Error a -> Bool
    isRateLimited (Left (AWS.TransportError (HttpExceptionRequest _ (StatusCodeException r _)))) = responseStatus r == status429
    isRateLimited _ = False
