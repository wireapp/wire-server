{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Stern.Types where

import Brig.Types
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Conversion
import Data.Json.Util
import Data.Range
import Data.Text
import Imports
import Galley.Types.Teams
import Galley.Types.Teams.Intra

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

-- These Datatypes are actually defined here
-- https://github.com/zinfra/galeb/blob/develop/src/main/java/com/wire/galeb/models/ConsentLog.java
-- https://github.com/zinfra/galeb/blob/develop/src/main/java/com/wire/galeb/models/ConsentResult.java
-- https://github.com/zinfra/galeb/blob/develop/src/main/java/com/wire/galeb/models/MarketoResult.java
-- However, given that we use this as a read-only and all info is to be displayed to clients, it seems
-- harmless (and easier) to just consume the whole object and simply use whatever galeb's JSON object is
newtype ConsentLog = ConsentLog
    { unConsentLog :: Object
    } deriving (Eq, Show, ToJSON, FromJSON)
newtype ConsentValue = ConsentValue
    { unConsentValue :: Object
    } deriving (Eq, Show, ToJSON, FromJSON)
newtype MarketoResult = MarketoResult
    { unMarketoResult :: Object
    } deriving (Eq, Show, ToJSON, FromJSON)
newtype UserProperties = UserProperties
    { unUserProperties :: M.HashMap PropertyKey PropertyValue
    } deriving (Eq, Show, ToJSON)
