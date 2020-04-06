{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Ropes.Aws
  ( -- * Env
    AccessKeyId (..),
    SecretAccessKey (..),
    Env,
    newEnv,
    getConfig,
    getCredentials,
    getManager,

    -- * Sending Requests
    sendRequest,

    -- * Re-exports
    Configuration (..),
    Credentials (..),
    ServiceConfiguration,
    NormalQuery,
    Transaction,
    ResponseMetadata,
  )
where

import Aws (Configuration (..), aws)
import qualified Aws
import Aws.Core
import Control.Error
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Control.Retry
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock
import Imports
import Network.HTTP.Client
import System.Logger (Logger)
import qualified System.Logger as Logger

-------------------------------------------------------------------------------
-- Config

newtype AccessKeyId
  = AccessKeyId
      {unKey :: ByteString}
  deriving (Eq, Show)

instance FromJSON AccessKeyId where
  parseJSON =
    withText "Aws.AccessKeyId" $
      pure . AccessKeyId . encodeUtf8

newtype SecretAccessKey
  = SecretAccessKey
      {unSecret :: ByteString}
  deriving (Eq)

instance Show SecretAccessKey where
  show _ = "AWS Secret hidden"

instance FromJSON SecretAccessKey where
  parseJSON =
    withText "Aws.SecretAccessKey" $
      pure . SecretAccessKey . encodeUtf8

data Auth
  = PermAuth Configuration
  | TempAuth (IORef Configuration)

-- | An environment for executing AWS requests. See 'sendRequest'.
data Env
  = Env
      { _auth :: !Auth,
        -- | Get the HTTP 'Manager' used by an 'Env'ironment.
        getManager :: !Manager
      }

-- | If credentials are supplied to this function, they are used to create the 'Env'
-- | Otherwise, it tries to discover AWS credentials by calling the underlying
-- | loadCredentialsDefault. If that does not succeed, if fallsback to instance metadata
newEnv :: Logger -> Manager -> Maybe (AccessKeyId, SecretAccessKey) -> IO Env
newEnv lgr mgr ks = do
  auth <- case ks of
    Just (k, s) -> permAuth =<< makeCredentials (unKey k) (unSecret s)
    Nothing -> discover
  return $ Env auth mgr
  where
    permAuth creds = PermAuth <$> newPermConfig lgr creds
    tempAuth = TempAuth <$> newTempConfig lgr mgr
    discover = loadCredentialsDefault >>= maybe tempAuth permAuth

-- | Get the currently used 'Credentials' from the current
-- 'Configuration' used by the given 'Env'.
getCredentials :: Env -> IO Credentials
getCredentials = fmap credentials . getConfig

-- | Get the currently used 'Configuration' of the given 'Env'.
getConfig :: Env -> IO Configuration
getConfig (Env (PermAuth c) _) = return c
getConfig (Env (TempAuth r) _) = readIORef r

-------------------------------------------------------------------------------
-- Sending Requests

sendRequest ::
  (MonadIO m, Transaction r a) =>
  Env ->
  ServiceConfiguration r NormalQuery ->
  r ->
  ResourceT m (ResponseMetadata a, a)
sendRequest env scfg req = transResourceT liftIO $ do
  cfg <- liftIO $ getConfig env
  rsp <- aws cfg scfg (getManager env) req
  a <- readResponseIO rsp
  return (responseMetadata rsp, a)

-------------------------------------------------------------------------------
-- Internals

newPermConfig :: Logger -> Credentials -> IO Configuration
newPermConfig lgr creds = return $ Configuration Timestamp creds (awsLog lgr) Nothing

newTempConfig :: Logger -> Manager -> IO (IORef Configuration)
newTempConfig lgr mgr = do
  Just (c, x) <- retrying x5 (const shouldRetry) (const tryMetadata)
  r <- newIORef c
  schedule r x
  return r
  where
    schedule r x = for_ x $ \expires -> do
      now <- getCurrentTime
      let secs = round (expires `diffUTCTime` now) - (60 * 30)
          msecs = secs * 1000 * 1000
      forkIO $ threadDelay msecs >> refresh r
    refresh r = do
      Just (c, x) <- retrying xInf (const shouldRetry) (const tryMetadata)
      writeIORef r c
      schedule r x
    shouldRetry = return . isNothing
    tryMetadata :: IO (Maybe (Configuration, Maybe UTCTime))
    tryMetadata = do
      Logger.info lgr $ Logger.msg ("Fetching instance metadata" :: String)
      r <- runExceptT . syncIO $ fromMetadata mgr
      case r of
        Left e -> Logger.err lgr (Logger.msg $ show e) >> return Nothing
        Right a -> do
          keys <- newIORef [] -- V4 signing keys used by the 'aws' package
          let (c, x) = mkCreds a keys
              cfg = Configuration Timestamp c (awsLog lgr) Nothing
          return $ Just (cfg, x)
    mkCreds (TempCredentials (AccessKeyId k) (SecretAccessKey s) (SessionToken t) expiry) keys =
      (Credentials k s keys (Just t), expiry)
    x5 = constantDelay 1000000 <> limitRetries 5 -- every second
    xInf = constantDelay (60 * 1000000) -- every minute

awsLog :: Logger -> Aws.LogLevel -> Text -> IO ()
awsLog lgr l m = Logger.log lgr (level l) (Logger.msg m)
  where
    level Aws.Debug = Logger.Debug
    level Aws.Info = Logger.Info
    level Aws.Warning = Logger.Warn
    level Aws.Error = Logger.Error

data TempCredentials
  = TempCredentials
      { _tmpKey :: AccessKeyId,
        _tmpSecret :: SecretAccessKey,
        _tmpToken :: SessionToken,
        _tmpExpiry :: Maybe UTCTime
      }

newtype SessionToken = SessionToken ByteString

newtype MetadataException = MetadataException Text
  deriving (Eq, Typeable)

instance Exception MetadataException

instance Show MetadataException where
  show (MetadataException e) = "Ropes.Aws.MetadataException: " ++ unpack e

fromMetadata :: Manager -> IO TempCredentials
fromMetadata mgr = do
  req <- parseUrlThrow $ C.unpack url
  role <- C.takeWhile (/= '\n') . LB.toStrict . responseBody <$> httpLbs req mgr
  cred <- eitherDecode . responseBody <$> httpLbs (req {path = (path req) <> "/" <> role}) mgr
  either (throwM . MetadataException . ("Failed to parse: " <>) . pack) return cred
  where
    url :: ByteString
    url = "http://instance-data/latest/meta-data/iam/security-credentials/"

instance FromJSON TempCredentials where
  parseJSON = withObject "credentials" $ \o ->
    TempCredentials
      <$> (AccessKeyId . encodeUtf8 <$> o .: "AccessKeyId")
      <*> (SecretAccessKey . encodeUtf8 <$> o .: "SecretAccessKey")
      <*> (SessionToken . encodeUtf8 <$> o .: "Token")
      <*> o .:? "Expiration"
