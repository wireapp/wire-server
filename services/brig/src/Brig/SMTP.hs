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

module Brig.SMTP where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Pool
import Data.Text (unpack)
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

initSMTP :: Logger -> Text -> Maybe PortNumber -> Maybe (Username, Password) -> SMTPConnType -> IO SMTP
initSMTP lg host port credentials connType = do
  -- Try to initiate a connection and fail badly right away in case of bad auth
  -- otherwise config errors will be detected "too late"
  (success, _) <- connect
  unless success $
    error "Failed to authenticate against the SMTP server"
  SMTP <$> createPool create destroy 1 5 5
  where
    connect = do
      conn <- case (connType, port) of
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
        (Just (Username u, Password p)) -> SMTP.authenticate SMTP.LOGIN (unpack u) (unpack p) conn
        _ -> return True
      return (ok, conn)
    create = do
      (ok, conn) <- connect
      if ok
        then Logger.log lg Logger.Debug (msg $ val "Established connection to: " +++ host)
        else Logger.log lg Logger.Warn (msg $ val "Failed to established connection, check your credentials to connect to: " +++ host)
      return conn
    destroy c = do
      SMTP.closeSMTP c
      Logger.log lg Logger.Debug (msg $ val "Closing connection to: " +++ host)

sendMail :: MonadIO m => SMTP -> Mail -> m ()
sendMail s m = liftIO $ withResource (s ^. pool) $ SMTP.sendMimeMail2 m
