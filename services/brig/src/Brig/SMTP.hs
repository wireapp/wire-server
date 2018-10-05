{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Brig.SMTP where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Pool
import Data.Text (Text, unpack)
import Network.Mail.Mime
import System.Logger.Class hiding (create)

import qualified Network.HaskellNet.SMTP     as SMTP
import qualified Network.HaskellNet.SMTP.SSL as SMTP
import qualified System.Logger               as Logger

newtype Username = Username Text
newtype Password = Password Text

data SMTP = SMTP
    { _pool :: !(Pool SMTP.SMTPConnection)
    }

data SMTPConnType = Plain
                  | TLS
                  | SSL
                  deriving (Eq, Show)

deriveJSON defaultOptions { constructorTagModifier = map toLower } ''SMTPConnType

makeLenses ''SMTP

initSMTP :: Logger -> Text -> Username -> Password -> SMTPConnType -> IO SMTP
initSMTP lg host (Username user) (Password pass) connType = do
    -- Try to initiate a connection and fail badly right away in case of bad auth
    -- otherwise config errors will be detected "too late"
    (success, _) <- connect
    unless success $
      error "Failed to authenticate against the SMTP server"
    SMTP <$> createPool create destroy 1 5 5
  where
    connect = do
      conn <- case connType of
                  Plain -> SMTP.connectSMTP (unpack host)
                  TLS   -> SMTP.connectSMTPSTARTTLS (unpack host)
                  SSL   -> SMTP.connectSMTPSSL (unpack host)
      ok   <- SMTP.authenticate SMTP.LOGIN (unpack user) (unpack pass) conn
      return (ok, conn)

    create = do
      (ok, conn) <- connect
      if ok
        then Logger.log lg Logger.Debug (msg $ val "Established connection to: " +++ host)
        else Logger.log lg Logger.Warn (msg $ val "Failed to established connection, check your credentials to connect to: " +++ host )
      return conn

    destroy c = do
      SMTP.closeSMTP c
      Logger.log lg Logger.Debug (msg $ val "Closing connection to: " +++ host)

sendMail :: MonadIO m => SMTP -> Mail -> m ()
sendMail s m = liftIO $ withResource (s^.pool) $ SMTP.sendMimeMail2 m
