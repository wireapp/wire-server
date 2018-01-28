{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Brig.Aws.Types
    ( -- * Config
      Account (..)
    , Region (..)
    , SesQueue (..)
    , InternalQueue (..)
    , BlacklistTable (..)
    , PreKeyTable (..)
    , Config
    , config
    , sesConfig
    -- , sqsConfig
    -- , sqsSesQueue
    -- , sqsInternalQueue
    , ddbConfig
    -- , ddbBlacklistTable
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
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Conversion
import Data.Id
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import qualified Aws.Core     as Aws
import qualified Aws.DynamoDb as Aws
import qualified Aws.Ses      as Aws
import qualified Aws.Sqs      as Aws

-------------------------------------------------------------------------------
-- Config

data Region = Ireland
            | Frankfurt

instance FromByteString Region where
    parser = parser >>= \t -> case t of
        "eu-west-1"    -> pure Ireland
        "eu-central-1" -> pure Frankfurt
        x              -> fail $ "Unsupported region " <> unpack x

instance Show Region where
    show Ireland   = "eu-west-1"
    show Frankfurt = "eu-central-1"

instance FromJSON Region where
    parseJSON = withText "aws-region" $
        maybe (fail "invalid region") return . fromByteString . encodeUtf8

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
    -- , _sqsConfig         :: !(Aws.SqsConfiguration Aws.NormalQuery)
    , _ddbConfig         :: !(Aws.DdbConfiguration Aws.NormalQuery)
    -- , _sqsSesQueue       :: !Aws.QueueName
    -- , _sqsInternalQueue  :: !Aws.QueueName
    -- , _ddbBlacklistTable :: !BlacklistTable
    , _ddbPreKeyTable    :: !PreKeyTable
    }

makeLenses ''Config

config :: Region
       -> Account
       -- -> SesQueue
       -- -> InternalQueue
       -- -> BlacklistTable
       -> PreKeyTable
       -> Config
config reg acc pkt =
    let ddb = regionSettings reg
        -- Note that `sesUsEast1` acts as a backup, in case `sesEuWest1` is down for some reason
        -- https://github.com/wireapp/wire-server/blob/develop/services/brig/src/Brig/Aws.hs#L144-L149
        -- Currently SES is only available in eu-west-1, us-east-1 and us-west-2 so not allowing it
        -- to be configured and is hardcoded to `eu-west-1` and `us-east-1` as a fallback
        ses = [Aws.sesHttpsPost Aws.sesEuWest1, Aws.sesHttpsPost Aws.sesUsEast1]
        -- sqq = Aws.QueueName (fromSesQueue squ) (fromAccount acc)
        -- iqq = Aws.QueueName (fromInternalQueue iqu) (fromAccount acc)
    in Config ses ddb pkt
  where
    regionSettings Ireland = Aws.ddbHttps Aws.ddbEuWest1
    --    ( Aws.sqs Aws.HTTPS Aws.sqsEndpointEu False
    regionSettings Frankfurt = Aws.ddbHttps Aws.ddbEuCentral1
    --    ( Aws.sqs Aws.HTTPS Aws.sqsEndpointEu { Aws.endpointHost = "eu-central-1.queue.amazonaws.com" } False

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
