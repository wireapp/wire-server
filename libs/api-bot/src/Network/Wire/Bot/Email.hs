{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wire.Bot.Email
  ( Mailbox (mailboxSettings),
    MailboxSettings (..),
    MailException (..),
    loadMailboxConfig,
    newMailbox,
    awaitActivationMail,
    awaitPasswordResetMail,
    awaitInvitationMail,
  )
where

import Codec.MIME.Parse
import Codec.MIME.Type
import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Id (InvitationId)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Pool (Pool, createPool, withResource)
import qualified Data.Text as T
import qualified Data.Text.Ascii as Ascii
import qualified Data.Text.Encoding as T
import Imports
import Network.HaskellNet.IMAP
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.SSL
import Network.Wire.Client.API.User

data MailboxSettings
  = MailboxSettings
      { mailboxHost :: String,
        mailboxUser :: Email,
        mailboxPassword :: String,
        mailboxConnections :: Int
      }

instance FromJSON MailboxSettings where
  parseJSON = withObject "mailbox-settings" $ \o ->
    MailboxSettings <$> o .: "host"
      <*> o .: "user"
      <*> o .: "pass"
      <*> o .: "conn"

data Mailbox
  = Mailbox
      { mailboxSettings :: MailboxSettings,
        mailboxPool :: Pool IMAPConnection
      }

data MailException
  = -- | Missing e-mail headers needed for automation.
    MissingEmailHeaders
  | -- | No email received within the timeout window.
    EmailTimeout
  deriving (Show, Typeable)

instance Exception MailException

loadMailboxConfig :: FilePath -> IO [Mailbox]
loadMailboxConfig p = do
  cfg <- LB.readFile p
  mbs <- either error return (eitherDecode' cfg) :: IO [MailboxSettings]
  mapM newMailbox mbs

newMailbox :: MailboxSettings -> IO Mailbox
newMailbox s@(MailboxSettings host usr pwd conns) =
  Mailbox s <$> createPool connect logout 1 60 (fromIntegral conns)
  where
    connect = do
      c <- connectIMAPSSLWithSettings host defaultSettingsIMAPSSL
      login c (show usr) pwd
      return c

-- | Awaits activation e-mail to arrive at a mailbox with
-- the designated recipient address.
awaitActivationMail ::
  Mailbox ->
  -- | Mailbox folders to search in, e.g. ["INBOX"]
  [String] ->
  -- | Expected "FROM"
  Email ->
  -- | Expected "TO"
  Email ->
  IO (NonEmpty (ActivationKey, ActivationCode))
awaitActivationMail mbox folders from to = do
  msgs <- awaitMail mbox folders from to "Activation"
  forM msgs $ \msg -> do
    let hdrs = mime_val_headers msg
    let keyHdr = find ((== "x-zeta-key") . paramName) hdrs
    let codeHdr = find ((== "x-zeta-code") . paramName) hdrs
    case liftM2 (,) keyHdr codeHdr of
      Just (k, c) ->
        return $
          ( ActivationKey $ Ascii.unsafeFromText $ paramValue k,
            ActivationCode $ Ascii.unsafeFromText $ paramValue c
          )
      Nothing -> throwIO MissingEmailHeaders

awaitPasswordResetMail ::
  Mailbox ->
  -- | Mailbox folders to search in, e.g. ["INBOX"]
  [String] ->
  -- | Expected "FROM"
  Email ->
  -- | Expected "TO"
  Email ->
  IO (NonEmpty (PasswordResetKey, PasswordResetCode))
awaitPasswordResetMail mbox folders from to = do
  msgs <- awaitMail mbox folders from to "PasswordReset"
  forM msgs $ \msg -> do
    let hdrs = mime_val_headers msg
    let keyHdr = find ((== "x-zeta-key") . paramName) hdrs
    let codeHdr = find ((== "x-zeta-code") . paramName) hdrs
    case liftM2 (,) keyHdr codeHdr of
      Just (k, c) ->
        return $
          ( PasswordResetKey $ Ascii.unsafeFromText $ paramValue k,
            PasswordResetCode $ Ascii.unsafeFromText $ paramValue c
          )
      Nothing -> throwIO MissingEmailHeaders

awaitInvitationMail ::
  Mailbox ->
  -- | Mailbox folders to search in, e.g. ["INBOX"]
  [String] ->
  -- | Expected "FROM"
  Email ->
  -- | Expected "TO"
  Email ->
  IO (NonEmpty InvitationId)
awaitInvitationMail mbox folders from to = do
  msgs <- awaitMail mbox folders from to "Invitation"
  forM msgs $ \msg -> do
    let hdrs = mime_val_headers msg
    let invHdr = find ((== "x-zeta-code") . paramName) hdrs
    case invHdr of
      Just i -> return . read . T.unpack $ paramValue i
      Nothing -> throwIO MissingEmailHeaders

awaitMail ::
  Mailbox ->
  -- | Mailbox folders to search in, e.g. ["INBOX"]
  [String] ->
  -- | Expected "FROM"
  Email ->
  -- | Expected "TO"
  Email ->
  -- | Expected "X-Zeta-Purpose"
  Text ->
  IO (NonEmpty MIMEValue)
awaitMail mbox folders from to purpose = go 0
  where
    sleep = 5000000 :: Int -- every 5 seconds
    timeout = sleep * 24 -- for up to 2 minutes
    go t = do
      msgs <- fetchMail mbox folders from to purpose -- TODO: Retry on (some?) exceptions
      case msgs of
        [] | t >= timeout -> throwIO EmailTimeout
        [] -> threadDelay sleep >> go (t + sleep)
        (m : ms) -> return (m :| ms)

fetchMail ::
  Mailbox ->
  -- | Mailbox folders to search in, e.g. ["INBOX"]
  [String] ->
  -- | Expected "FROM"
  Email ->
  -- | Expected "TO"
  Email ->
  -- | Expected "X-Zeta-Purpose"
  Text ->
  IO [MIMEValue]
fetchMail mbox folders from to purpose = withResource (mailboxPool mbox) $ \c -> do
  msgIds <- concat <$> forM folders (searchMail c)
  msgs <- mapM (fetch c) msgIds
  return $ map (parseMIMEMessage . T.decodeLatin1) msgs
  where
    searchMail c folder = do
      select c folder
      search
        c
        [ NOTs (FLAG Seen),
          FROMs (T.unpack $ fromEmail from),
          TOs (T.unpack $ fromEmail to),
          HEADERs "X-Zeta-Purpose" (show purpose)
        ]
