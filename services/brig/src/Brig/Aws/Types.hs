{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Brig.Aws.Types
    ( -- * Config
      Account (..)
    , SesQueue (..)
    , InternalQueue (..)
    , BlacklistTable (..)
    , PreKeyTable (..)
    , Config
    , config
    , sesConfig
    , sqsConfig
    , sqsSesQueue
    , sqsInternalQueue
    , ddbConfig
    , ddbBlacklistTable
    , ddbPreKeyTable

      -- * SES Notification
    , SESNotification (..)
    , SESBounceType (..)
      -- * Internal Notification
    , InternalNotification (..)
    ) where

import Brig.Types (Email (..))
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Id
import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Aws.Core     as Aws
import qualified Aws.DynamoDb as Aws
import qualified Aws.Ses      as Aws
import qualified Aws.Sqs      as Aws

-------------------------------------------------------------------------------
-- Config

newtype SesQueue = SesQueue
    { fromSesQueue :: Text }
    deriving (Eq, Show)

newtype InternalQueue = InternalQueue
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

data Config = Config
    { _sesConfig         :: ![Aws.SesConfiguration Aws.NormalQuery]
    , _sqsConfig         :: !(Aws.SqsConfiguration Aws.NormalQuery)
    , _ddbConfig         :: !(Aws.DdbConfiguration Aws.NormalQuery)
    , _sqsSesQueue       :: !Aws.QueueName
    , _sqsInternalQueue  :: !Aws.QueueName
    , _ddbBlacklistTable :: !BlacklistTable
    , _ddbPreKeyTable    :: !PreKeyTable
    }

makeLenses ''Config

config :: Account
       -> SesQueue
       -> InternalQueue
       -> BlacklistTable
       -> PreKeyTable
       -> Config
config acc squ iqu blt pkt =
    let ses = [Aws.sesHttpsPost Aws.sesEuWest1, Aws.sesHttpsPost Aws.sesUsEast1]
        sqs = Aws.sqs Aws.HTTPS Aws.sqsEndpointEu False
        ddb = Aws.ddbHttps (Aws.Region "dynamodb.eu-west-1.amazonaws.com" "eu-west-1")
        sqq = Aws.QueueName (fromSesQueue squ) (fromAccount acc)
        iqq = Aws.QueueName (fromInternalQueue iqu) (fromAccount acc)
    in Config ses sqs ddb sqq iqq blt pkt

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
            x           -> fail ("Brig.Aws: Unexpected notification type" ++ show x)

data InternalNotification
    = DeleteUser !UserId
    deriving (Eq, Show)

data InternalNotificationType
    = UserDeletion
    deriving (Eq, Show)

instance FromJSON InternalNotificationType where
    parseJSON "user.delete" = return UserDeletion
    parseJSON x             = fail $ "InternalNotificationType: Unknown type " <> show x

instance ToJSON InternalNotificationType where
    toJSON UserDeletion = "user.delete" 

instance FromJSON InternalNotification where
    parseJSON = withObject "InternalNotification" $ \o -> do
        t <- o .: "type"
        case (t :: InternalNotificationType) of
            UserDeletion -> DeleteUser <$> o .: "user"

instance ToJSON InternalNotification where
    toJSON (DeleteUser u) = object 
        [ "user" .= u
        , "type" .= UserDeletion
        ]
