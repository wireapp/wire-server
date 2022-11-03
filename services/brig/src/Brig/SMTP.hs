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

module Brig.SMTP
  ( sendMail,
    initSMTP,
    sendMail',
    initSMTP',
    SMTPConnType (..),
    SMTP (..),
    Username (..),
    Password (..),
    SMTPPoolException (..),
  )
where

import qualified Control.Exception as CE (throw)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Trans.Except
import Control.Timeout (timeout)
import Data.Aeson
import Data.Aeson.TH
import Data.Either.Extra
import Data.Pool
import Data.Text (unpack)
import Data.Time.Units
import Imports
import qualified Network.HaskellNet.SMTP as SMTP
import qualified Network.HaskellNet.SMTP.SSL as SMTP
import Network.Mail.Mime
import Network.Socket (PortNumber)
import qualified System.Logger as Logger
import System.Logger.Class hiding (create)

newtype Username = Username Text

newtype Password = Password Text

data SMTP = SMTP
  { _pool :: !(Pool SMTP.SMTPConnection)
  }

data SMTPConnType
  = Plain
  | TLS
  | SSL
  deriving (Eq, Show)

deriveJSON defaultOptions {constructorTagModifier = map toLower} ''SMTPConnType

makeLenses ''SMTP

data SMTPFailure = Unauthorized | ConnectionTimeout | CaughtException SomeException
  deriving (Show)

data SMTPPoolException = SMTPUnauthorized | SMTPConnectionTimeout
  deriving (Eq, Show)

instance Exception SMTPPoolException

-- | Initiate the `SMTP` connection pool
--
-- Throws exceptions when the SMTP server is unreachable, authentication fails,
-- a timeout happens and on every other network failure.
--
-- `defaultTimeoutDuration` is used as timeout duration for all actions.
initSMTP ::
  Logger ->
  Text ->
  Maybe PortNumber ->
  Maybe (Username, Password) ->
  SMTPConnType ->
  IO SMTP
initSMTP = initSMTP' defaultTimeoutDuration

-- | `initSMTP` with configurable timeout duration
--
-- This is mostly useful for testing. (We don't want to waste the amount of
-- `defaultTimeoutDuration` in tests with waiting.)
initSMTP' ::
  (TimeUnit t) =>
  t ->
  Logger ->
  Text ->
  Maybe PortNumber ->
  Maybe (Username, Password) ->
  SMTPConnType ->
  IO SMTP
initSMTP' timeoutDuration lg host port credentials connType = do
  -- Try to initiate a connection and fail badly right away in case of bad auth
  -- otherwise config errors will be detected "too late"
  res <- runExceptT establishConnection
  logResult lg ("Checking test connection to " ++ unpack host ++ " on startup") res
  -- Ensure that the logs are written: In case of failure, the errors thrown
  -- below will kill the app (which could otherwise leave the logs unwritten).
  flush lg
  case res of
    Left e ->
      error $ "Failed to establish test connection with SMTP server: " ++ show e
    Right con ->
      either
        (error "Failed to establish test connection with SMTP server.")
        (const (SMTP <$> createPool create destroy 1 5 5))
        =<< do
          r <- ensureSMTPConnectionTimeout timeoutDuration (SMTP.gracefullyCloseSMTP con)
          logResult lg "Closing test connection on startup" r
          pure r
  where
    liftSMTP :: IO a -> ExceptT SMTPFailure IO a
    liftSMTP action = ExceptT $ ensureSMTPConnectionTimeout timeoutDuration action

    establishConnection :: ExceptT SMTPFailure IO SMTP.SMTPConnection
    establishConnection = do
      conn <- liftSMTP $ case (connType, port) of
        (Plain, Nothing) -> SMTP.connectSMTP (unpack host)
        (Plain, Just p) -> SMTP.connectSMTPPort (unpack host) p
        (TLS, Nothing) -> SMTP.connectSMTPSTARTTLS (unpack host)
        (TLS, Just p) ->
          SMTP.connectSMTPSTARTTLSWithSettings (unpack host) $
            SMTP.defaultSettingsSMTPSTARTTLS {SMTP.sslPort = p}
        (SSL, Nothing) -> SMTP.connectSMTPSSL (unpack host)
        (SSL, Just p) ->
          SMTP.connectSMTPSSLWithSettings (unpack host) $
            SMTP.defaultSettingsSMTPSSL {SMTP.sslPort = p}
      ok <- case credentials of
        (Just (Username u, Password p)) ->
          liftSMTP $
            SMTP.authenticate SMTP.LOGIN (unpack u) (unpack p) conn
        _ -> pure True
      if ok
        then pure conn
        else throwE Unauthorized

    create :: IO SMTP.SMTPConnection
    create = do
      res <- runExceptT establishConnection
      logResult lg "Creating pooled SMTP connection" res
      throwOnLeft res

    destroy :: SMTP.SMTPConnection -> IO ()
    destroy c =
      (ensureSMTPConnectionTimeout timeoutDuration . SMTP.gracefullyCloseSMTP) c
        >>= void . logResult lg ("Closing pooled SMTP connection to " ++ unpack host)

throwOnLeft :: MonadIO m => Either SMTPFailure a -> m a
throwOnLeft = \case
  Left Unauthorized -> CE.throw SMTPUnauthorized
  Left ConnectionTimeout -> CE.throw SMTPConnectionTimeout
  Left (CaughtException e) -> CE.throw e
  Right a -> pure a

logResult :: MonadIO m => Logger -> String -> Either SMTPFailure c -> m ()
logResult lg actionString res =
  case res of
    Left Unauthorized -> do
      Logger.log
        lg
        Logger.Warn
        (msg $ concatToVal actionString "Failed to established connection, check your credentials.")
    Left ConnectionTimeout -> do
      Logger.log lg Logger.Warn (msg $ concatToVal actionString "Connection timeout.")
    Left (CaughtException e) -> do
      Logger.log lg Logger.Warn (msg $ concatToVal actionString ("Caught exception : " ++ show e))
    Right _ -> do
      Logger.log lg Logger.Debug (msg $ concatToVal actionString "Succeeded.")
  where
    concatToVal :: ToBytes s1 => s1 -> String -> Builder
    concatToVal a b = a +++ (" : " :: String) +++ b

-- | Default timeout for all actions
--
-- It's arguable if this shouldn't become a configuration setting in future.
-- It's an almost obscenely long duration, as we just want to make sure SMTP
-- servers / network components aren't playing tricks to us. Other cases should
-- be handled by the network libraries themselves.
defaultTimeoutDuration :: Second
defaultTimeoutDuration = 15 :: Second

ensureSMTPConnectionTimeout :: (MonadIO m, MonadCatch m, TimeUnit t) => t -> m a -> m (Either SMTPFailure a)
ensureSMTPConnectionTimeout timeoutDuration action =
  catch
    (maybe (Left ConnectionTimeout) Right <$> timeout timeoutDuration action)
    (\(e :: SomeException) -> pure (Left (CaughtException e)))

-- | Send a `Mail` via an existing `SMTP` connection pool
--
-- Throws exceptions when the SMTP server is unreachable, authentication fails,
-- a timeout happens and on every other network failure.
--
-- `defaultTimeoutDuration` is used as timeout duration for all actions.
sendMail :: (MonadIO m, MonadCatch m) => Logger -> SMTP -> Mail -> m ()
sendMail = sendMail' defaultTimeoutDuration

-- | `sendMail` with configurable timeout duration
--
-- This is mostly useful for testing. (We don't want to waste the amount of
-- `defaultTimeoutDuration` in tests with waiting.)
sendMail' :: (MonadIO m, MonadCatch m, TimeUnit t) => t -> Logger -> SMTP -> Mail -> m ()
sendMail' timeoutDuration lg s m = liftIO $ withResource (s ^. pool) sendMail''
  where
    sendMail'' :: SMTP.SMTPConnection -> IO ()
    sendMail'' c = ensureSMTPConnectionTimeout timeoutDuration (SMTP.sendMail m c) >>= handleError

    handleError :: MonadIO m => Either SMTPFailure a -> m ()
    handleError r =
      logResult lg "Sending mail via SMTP" r
        >> (void . throwOnLeft) r
