{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Brig.AWS.Types
    ( -- * Config
      Account (..)
    , SesQueueUrl (..)
    , InternalQueueUrl (..)
    , BlacklistTable (..)
    , PreKeyTable (..)

      -- * SES Notification
    , SESNotification (..)
    , SESBounceType (..)
    ) where

import Brig.Types (Email (..))
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text (Text)

-------------------------------------------------------------------------------
-- Config

newtype SesQueueUrl = SesQueueUrl
    { fromSesQueue :: Text }
    deriving (Eq, Show)

-- TODO: not used anywhere
newtype InternalQueueUrl = InternalQueueUrl
    { fromInternalQueue :: Text }
    deriving (Eq, Show)

newtype Account = Account
    { fromAccount :: Text }
    deriving (Eq, Show)

newtype BlacklistTable = BlacklistTable
    { blacklistTableName :: Text }
    deriving (Eq, Show)

newtype PreKeyTable = PreKeyTable
    { preKeyTableName :: Text }
    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Notifications

data SESNotification
    = MailBounce !SESBounceType [Email]
    | MailComplaint [Email]
    deriving (Eq, Show)

data SESBounceType
    = BounceUndetermined
    | BouncePermanent
    | BounceTransient
    deriving (Eq, Show)

instance FromJSON SESBounceType where
    parseJSON "Undetermined" = return BounceUndetermined
    parseJSON "Permanent"    = return BouncePermanent
    parseJSON "Transient"    = return BounceTransient
    parseJSON x              = fail $ "Unknown type: " <> show x

instance FromJSON SESNotification where
    parseJSON = withObject "SESNotification" $ \o -> do
        t <- o .: "notificationType"
        case (t :: Text) of
            "Bounce" -> do
                b  <- o .: "bounce"
                bt <- b .: "bounceType"
                br <- b .: "bouncedRecipients"
                em <- mapM (\r -> r .: "emailAddress") br
                return $! MailBounce bt em
            "Complaint" -> do
                c  <- o .: "complaint"
                cr <- c .: "complainedRecipients"
                em <- mapM (\r -> r .: "emailAddress") cr
                return $! MailComplaint em
            x           -> fail ("Brig.AWS: Unexpected notification type" ++ show x)
