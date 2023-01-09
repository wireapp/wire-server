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
import Control.Timeout (timeout)
import Data.Aeson
import Data.Aeson.TH
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

data SMTPPoolException = SMTPUnauthorized | SMTPConnectionTimeout
  deriving (Eq, Show)

instance Exception SMTPPoolException

-- | Initiate the `SMTP` connection pool
--
-- Throws exceptions when the SMTP server is unreachable, authentication fails,
-- a timeout happens, or on every other network failure.
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
  -- Try to initiate a connection and fail badly right away in case of bad auth.
  -- Otherwise, config errors will be detected "too late".
  con <-
    catch
      ( logExceptionOrResult
          lg
          ("Checking test connection to " ++ unpack host ++ " on startup")
          establishConnection
      )
      ( \(e :: SomeException) -> do
          -- Ensure that the logs are written: In case of failure, the error thrown
          -- below will kill the app (which could otherwise leave the logs unwritten).
          flush lg
          error $ "Failed to establish test connection with SMTP server: " ++ show e
      )
  catch
    ( logExceptionOrResult lg "Closing test connection on startup" $
        ensureSMTPConnectionTimeout timeoutDuration (SMTP.gracefullyCloseSMTP con)
    )
    ( \(e :: SomeException) -> do
        -- Ensure that the logs are written: In case of failure, the error thrown
        -- below will kill the app (which could otherwise leave the logs unwritten).
        flush lg
        error $ "Failed to close test connection with SMTP server: " ++ show e
    )
  SMTP <$> createPool create destroy 1 5 5
  where
    ensureTimeout :: IO a -> IO a
    ensureTimeout = ensureSMTPConnectionTimeout timeoutDuration

    establishConnection :: IO SMTP.SMTPConnection
    establishConnection = do
      conn <- ensureTimeout $ case (connType, port) of
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
          ensureTimeout $
            SMTP.authenticate SMTP.LOGIN (unpack u) (unpack p) conn
        _ -> pure True
      if ok
        then pure conn
        else CE.throw SMTPUnauthorized

    create :: IO SMTP.SMTPConnection
    create =
      logExceptionOrResult
        lg
        ("Creating pooled SMTP connection to " ++ unpack host)
        establishConnection

    destroy :: SMTP.SMTPConnection -> IO ()
    destroy c =
      logExceptionOrResult lg ("Closing pooled SMTP connection to " ++ unpack host) $
        (ensureTimeout . SMTP.gracefullyCloseSMTP) c

logExceptionOrResult :: (MonadIO m, MonadCatch m) => Logger -> String -> m a -> m a
logExceptionOrResult lg actionString action = do
  res <-
    catches
      action
      [ Handler
          ( \(e :: SMTPPoolException) -> do
              let resultLog = case e of
                    SMTPUnauthorized ->
                      ("Failed to establish connection, check your credentials." :: String)
                    SMTPConnectionTimeout -> ("Connection timeout." :: String)
              doLog Logger.Warn resultLog
              CE.throw e
          ),
        Handler
          ( \(e :: SomeException) -> do
              doLog Logger.Warn ("Caught exception : " ++ show e)
              CE.throw e
          )
      ]
  doLog Logger.Debug ("Succeeded." :: String)
  pure res
  where
    doLog :: MonadIO m => Logger.Level -> String -> m ()
    doLog lvl result =
      let msg' = msg ("SMTP connection result" :: String)
       in Logger.log lg lvl (msg' . field "action" actionString . field "result" result)

-- | Default timeout for all actions
--
-- It's arguable if this shouldn't become a configuration setting in future.
-- It's an almost obscenely long duration, as we just want to make sure SMTP
-- servers / network components aren't playing tricks on us. Other cases should
-- be handled by the network libraries themselves.
defaultTimeoutDuration :: Second
defaultTimeoutDuration = 15

-- | Wrapper function for `SMTP` network actions
--
-- This function ensures that @action@ finishes in a given period of time.
-- Throws on a timeout. Exceptions of @action@ are propagated (re-thrown).
ensureSMTPConnectionTimeout :: (MonadIO m, MonadCatch m, TimeUnit t) => t -> m a -> m a
ensureSMTPConnectionTimeout timeoutDuration action =
  timeout timeoutDuration action >>= maybe (CE.throw SMTPConnectionTimeout) pure

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
    sendMail'' c =
      logExceptionOrResult lg "Sending mail via SMTP" $
        ensureSMTPConnectionTimeout timeoutDuration (SMTP.sendMail m c)
