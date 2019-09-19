{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Stern.Types where

import Brig.Types
import Brig.Types.Intra
import Brig.Types.User.Auth
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Conversion
import Data.Json.Util
import Data.Range
import "swagger2" Data.Swagger
import Data.Text
import Imports
import Galley.Types
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Gundeck.Types.Notification

import qualified Data.HashMap.Strict as M

newtype TeamMemberInfo = TeamMemberInfo { tm :: TeamMember }

instance ToJSON TeamMemberInfo where
    toJSON (TeamMemberInfo m) =
        let Object o = teamMemberJson (const True) m
        in Object $ M.insert "can_update_billing" (Bool (hasPermission m SetBilling))
                  $ M.insert "can_view_billing"   (Bool (hasPermission m GetBilling))
                  $ o

data TeamInfo = TeamInfo
    { tiData    :: TeamData
    , tiMembers :: [TeamMemberInfo]
    }

instance ToJSON TeamInfo where
    toJSON (TeamInfo d m) = object
        [ "info"    .= d
        , "members" .= m
        ]

newtype UserProperties = UserProperties
    { unUserProperties :: M.HashMap PropertyKey PropertyValue
    } deriving (Eq, Show, ToJSON)

-- | NOTE: The following datatypes are defined by services used only internally at Wire
-- related to billing services and others and are not relevant for generic wire-server
-- installations.
--
-- For reference, these models are defined here (note that these are private repos)
-- https://github.com/zinfra/galeb/blob/develop/src/main/java/com/wire/galeb/models/ConsentLog.java
-- https://github.com/zinfra/galeb/blob/develop/src/main/java/com/wire/galeb/models/ConsentResult.java
-- https://github.com/zinfra/galeb/blob/develop/src/main/java/com/wire/galeb/models/MarketoResult.java
-- Note that we define them simply as JSON objects since we use them as a read-only and all info is to
-- be displayed to clients. Thus, it seems harmless (and easier) to just consume the whole object and
-- simply use whatever galeb's JSON object looks like
newtype ConsentLog = ConsentLog
    { unConsentLog :: Object
    } deriving (Eq, Show, ToJSON, FromJSON)
newtype ConsentValue = ConsentValue
    { unConsentValue :: Object
    } deriving (Eq, Show, ToJSON, FromJSON)
newtype MarketoResult = MarketoResult
    { unMarketoResult :: Object
    } deriving (Eq, Show, ToJSON, FromJSON)

newtype InvoiceId = InvoiceId { unInvoiceId :: Text }
    deriving (Eq, Show, ToByteString, FromByteString, ToJSON, FromJSON)

data TeamBillingInfo = TeamBillingInfo
    { tbiFirstname :: Text
    , tbiLastname  :: Text
    , tbiStreet    :: Text
    , tbiZip       :: Text
    , tbiCity      :: Text
    , tbiCountry   :: Text
    , tbiCompany   :: Maybe Text
    , tbiState     :: Maybe Text
    } deriving (Eq, Show)

deriveJSON toJSONFieldName ''TeamBillingInfo

data TeamBillingInfoUpdate = TeamBillingInfoUpdate
    { tbiuFirstname :: Maybe (Range 1 256 Text)
    , tbiuLastname  :: Maybe (Range 1 256 Text)
    , tbiuStreet    :: Maybe (Range 1 256 Text)
    , tbiuZip       :: Maybe (Range 1 16  Text)
    , tbiuCity      :: Maybe (Range 1 256 Text)
    , tbiuCountry   :: Maybe (Range 1 256 Text)
    , tbiuCompany   :: Maybe (Range 1 256 Text)
    , tbiuState     :: Maybe (Range 1 256 Text)
    } deriving (Eq, Show)

deriveJSON toJSONFieldName ''TeamBillingInfoUpdate

data SetLegalHoldStatus = SetLegalHoldDisabled | SetLegalHoldEnabled
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

deriveJSON toJSONFieldName ''SetLegalHoldStatus

data SetSSOStatus = SetSSODisabled | SetSSOEnabled
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

deriveJSON toJSONFieldName ''SetSSOStatus

-- FUTUREWORK: This will be removed as soon as this is ported to another tool
data UserMetaInfo = UserMetaInfo
    { _umiAccount       :: UserAccount
    , _umiConnections   :: [UserConnection]
    , _umiConversations :: [Conversation]
    , _umiNotifications :: [QueuedNotification]
    , _umiClients       :: [Client]
    , _umiConsent       :: ConsentValue
    , _umiConsentLog    :: ConsentLog
    , _umiCookies       :: CookieList
    , _umiProperties    :: UserProperties
    , _umiMarketo       :: MarketoResult
    }

instance ToJSON UserMetaInfo where
  toJSON umi = object
      [ "account"       .= _umiAccount umi
      , "connections"   .= _umiConnections umi
      , "conversations" .= _umiConversations umi
      , "notifications" .= _umiNotifications umi
      , "clients"       .= _umiClients umi
      , "consent"       .= _umiConsent umi
      , "consent_log"   .= _umiConsentLog umi
      , "cookies"       .= _umiCookies umi
      , "properties"    .= _umiProperties umi
      , "marketo"       .= _umiMarketo umi
      ]

instance ToSchema UserMetaInfo where
  declareNamedSchema = undefined

data UserConnectionsByStatus = UserConnectionsByStatus
    { _ucbsAccepted :: Int
    , _ucbsSent     :: Int
    , _ucbsPending  :: Int
    , _ucbsBlocked  :: Int
    , _ucbsIgnored  :: Int
    , _ucbsTotal    :: Int
    }

instance ToJSON UserConnectionsByStatus where
  toJSON ucbs = object
      [ "accepted"    .= _ucbsAccepted ucbs
      , "sent"        .= _ucbsSent ucbs
      , "pending"     .= _ucbsPending ucbs
      , "blocked"     .= _ucbsBlocked ucbs
      , "ignored"     .= _ucbsIgnored ucbs
      , "total"       .= _ucbsTotal ucbs
      ]

instance ToSchema UserConnectionsByStatus where
  declareNamedSchema = undefined
