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

module Brig.SMTP where

import qualified Control.Exception as CE (handle, throw)
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

data SMTPPoolException = SMTPUnauthorized | SMTPConnectionTimeout
  deriving (Show)

instance Exception SMTPPoolException

initSMTP :: Logger -> Text -> Maybe PortNumber -> Maybe (Username, Password) -> SMTPConnType -> IO SMTP
initSMTP lg host port credentials connType = do
  -- Try to initiate a connection and fail badly right away in case of bad auth
  -- otherwise config errors will be detected "too late"
  res <- runExceptT establishConnection
  logResult res
  case res of
    Left Unauthorized ->
      error "Failed to authenticate against the SMTP server"
    Left ConnectionTimeout ->
      error "Failed to connect to SMTP server. Connection timeout."
    Left (CaughtException e) ->
      error $ "Caught exception while trying to connect to SMTP server : " ++ show e
    Right con -> do
      -- TODO: gracefullyCloseSMTP may throw
      SMTP.gracefullyCloseSMTP con
      SMTP <$> createPool create destroy 1 5 5
  where
    liftSMTP :: IO a -> ExceptT SMTPFailure IO a
    liftSMTP action =
      ExceptT $
        CE.handle (\e -> (pure . Left . CaughtException) e) $
          maybeToEither ConnectionTimeout <$> ensureSMTPConnectionTimeout lg action

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
        (Just (Username u, Password p)) -> liftSMTP $ SMTP.authenticate SMTP.LOGIN (unpack u) (unpack p) conn
        _ -> pure True
      if ok
        then pure conn
        else throwE Unauthorized

    create :: IO SMTP.SMTPConnection
    create = do
      res <- runExceptT establishConnection
      logResult res
      case res of
        Left Unauthorized -> do
          CE.throw SMTPUnauthorized
        Left ConnectionTimeout -> do
          CE.throw SMTPConnectionTimeout
        Left (CaughtException e) -> do
          CE.throw e
        Right con -> do
          pure con

    logResult :: MonadIO m => Either SMTPFailure SMTP.SMTPConnection -> m ()
    logResult res =
      case res of
        Left Unauthorized -> do
          Logger.log lg Logger.Warn (msg $ val "Failed to established connection, check your credentials to connect to: " +++ host)
        Left ConnectionTimeout -> do
          Logger.log lg Logger.Warn (msg $ (val "Failed to connect to : " +++ host) +++ val " . Connection timeout.")
        Left (CaughtException e) -> do
          Logger.log lg Logger.Warn (msg $ val "Caught exception while trying to connect to SMTP server : " +++ show e)
        Right _ -> do
          Logger.log lg Logger.Debug (msg $ val "Established connection to: " +++ host)

    destroy c = do
      Logger.log lg Logger.Debug (msg $ val "Closing connection to: " +++ host)
      -- TODO: gracefullyCloseSMTP may throw
      r <- ensureSMTPConnectionTimeout lg $ SMTP.gracefullyCloseSMTP c
      if isJust r
        then Logger.log lg Logger.Debug (msg $ val "Closed connection to: " +++ host)
        else Logger.log lg Logger.Debug (msg $ val "Closing connection to " +++ host +++ val " timed out")

sendMail :: (MonadIO m, MonadCatch m) => Logger -> SMTP -> Mail -> m ()
sendMail lg s m = liftIO $ withResource (s ^. pool) sendMail'
  where
    sendMail' c = ensureSMTPConnectionTimeout lg (SMTP.sendMail m c) >>= handleTimeout
    handleTimeout r =
      if isJust r
        then do
          Logger.log lg Logger.Debug (msg $ val "Sent mail")
          pure ()
        else do
          Logger.log lg Logger.Debug (msg $ val "Sending mail timed out. Mail not sent.")
          CE.throw SMTPConnectionTimeout

ensureSMTPConnectionTimeout :: (MonadIO m, MonadCatch m) => Logger -> m a -> m (Maybe a)
ensureSMTPConnectionTimeout lg action =
  catch
    (timeout (15 :: Second) action)
    ( \(e :: SomeException) ->
        Logger.log lg Logger.Warn (msg $ val "Caught exception while trying to connect to SMTP server : " +++ show e)
          >> pure Nothing
    )
