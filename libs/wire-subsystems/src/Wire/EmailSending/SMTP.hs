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

module Wire.EmailSending.SMTP
  ( initSMTP,
    emailViaSMTPInterpreter,
    sendMailWithDuration,
    initSMTPWithTimeout,
    SMTPConnType (..),
    SMTP (..),
    Credential (..),
    SMTPPoolException (..),
  )
where

import Control.Concurrent.Async (wait, withAsyncWithUnmask)
import Control.Exception qualified as CE (throw)
import Control.Monad.Catch
import Control.Timeout (timeout)
import Data.Aeson
import Data.Aeson.TH
import Data.Pool
import Data.Text (unpack)
import Data.Time.Units
import Imports
import Network.HaskellNet.SMTP qualified as SMTP
import Network.HaskellNet.SMTP.SSL qualified as SMTP
import Network.Mail.Mime
import Network.Socket (PortNumber)
import Polysemy
import System.Logger qualified as Logger
import System.Logger.Class hiding (create)
import Wire.EmailSending

emailViaSMTPInterpreter :: (Member (Embed IO) r) => Logger -> SMTP -> InterpreterFor EmailSending r
emailViaSMTPInterpreter logger smtp = interpret \case
  SendMail mail -> sendMailImpl logger smtp mail

data Credential
  = -- | username and password
    BasicAuth Text Text
  | -- | username and token
    XAUTH2Token Text Text

data SMTP = SMTP {pool :: !(Pool SMTP.SMTPConnection)}

data SMTPConnType
  = Plain
  | TLS
  | SSL
  deriving (Eq, Show)

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
  Maybe Credential ->
  SMTPConnType ->
  IO SMTP
initSMTP = initSMTPWithTimeout defaultTimeoutDuration

-- | `initSMTP` with configurable timeout duration
--
-- This is mostly useful for testing. (We don't want to waste the amount of
-- `defaultTimeoutDuration` in tests with waiting.)
initSMTPWithTimeout ::
  (TimeUnit t) =>
  t ->
  Logger ->
  Text ->
  Maybe PortNumber ->
  Maybe Credential ->
  SMTPConnType ->
  IO SMTP
initSMTPWithTimeout timeoutDuration lg host port credentials connType = do
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
  SMTP <$> newPool (setNumStripes (Just 1) (defaultPoolConfig create destroy 5 5))
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
        Just (BasicAuth u p) ->
          ensureTimeout $
            SMTP.authenticate SMTP.LOGIN (unpack u) (unpack p) conn
        Just (XAUTH2Token u t) ->
          ensureTimeout $
            SMTP.authenticate SMTP.XOAUTH2 (unpack u) (unpack t) conn
        Nothing -> pure True
      if ok
        then pure conn
        else CE.throw SMTPUnauthorized

    create :: IO SMTP.SMTPConnection
    create =
      logExceptionOrResult
        lg
        ("Creating pooled SMTP connection to " ++ unpack host)
        establishConnection

    -- NOTE: because `Data.Pool` masks the async exceptions for the resource deallocation function,
    --       the timeout function called in `ensureTimeOut` will be masked as well and cannot
    --       ever terminate, so `destroy` itself never terminates
    destroy :: SMTP.SMTPConnection -> IO ()
    destroy c =
      withAsyncWithUnmask
        do
          \unmask -> do
            logExceptionOrResult lg ("Closing pooled SMTP connection to " ++ unpack host) $
              unmask do
                ensureTimeout $ SMTP.gracefullyCloseSMTP c
        do wait

logExceptionOrResult :: (MonadIO m, MonadCatch m) => Logger -> String -> m a -> m a
logExceptionOrResult lg actionString action = do
  res <-
    catches
      action
      [ Handler
          \(e :: SMTPPoolException) -> do
            let resultLog = case e of
                  SMTPUnauthorized ->
                    ("Failed to establish connection, check your credentials." :: String)
                  SMTPConnectionTimeout -> ("Connection timeout." :: String)
            doLog Logger.Warn resultLog
            CE.throw e,
        Handler
          \(e :: SomeException) -> do
            doLog Logger.Warn ("Caught exception : " ++ show e)
            CE.throw e
      ]
  doLog Logger.Debug ("Succeeded." :: String)
  pure res
  where
    doLog :: (MonadIO m) => Logger.Level -> String -> m ()
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
sendMailImpl :: (MonadIO m) => Logger -> SMTP -> Mail -> m ()
sendMailImpl = sendMailWithDuration defaultTimeoutDuration

-- | `sendMail` with configurable timeout duration
--
-- This is mostly useful for testing. (We don't want to waste the amount of
-- `defaultTimeoutDuration` in tests with waiting.)
sendMailWithDuration :: forall t m. (MonadIO m, TimeUnit t) => t -> Logger -> SMTP -> Mail -> m ()
sendMailWithDuration timeoutDuration lg smtp m = liftIO $ withResource smtp.pool sendMailWithConn
  where
    sendMailWithConn :: SMTP.SMTPConnection -> IO ()
    sendMailWithConn c =
      logExceptionOrResult lg "Sending mail via SMTP" $
        ensureSMTPConnectionTimeout timeoutDuration (SMTP.sendMail m c)

deriveJSON defaultOptions {constructorTagModifier = map toLower} ''SMTPConnType
