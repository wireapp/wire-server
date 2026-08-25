{-# LANGUAGE TemplateHaskell #-}

module Wire.UserStore.Migration.Types where

import Cassandra.Util
import Data.Handle
import Data.Id
import Data.Json.Util
import Data.Time
import Database.CQL.Protocol (Record (..), TupleType, recordInstance)
import Imports
import Wire.API.Password
import Wire.API.User
import Wire.API.User.RichInfo

data RawUserData = RawUserData
  { id :: UserId,
    user :: UserRowCass,
    richInfo :: Maybe RichInfoAssocList,
    serviceConv :: Maybe ServiceConv,
    handleClaimValidity :: HandleClaimValidity
  }

data HandleClaimValidity
  = HandleClaimValid
  | HandleNotClaimed
  | HandleClaimedByAnotherUser UserId

-- | Some fields are read as 'Maybe' even if they're supposed to always be
-- there. This is to deal with potential old data in the DB.
data UserRowCass = UserRowCass
  { accentId :: Maybe ColourId,
    activated :: Maybe Bool,
    country :: Maybe Country,
    email :: Maybe EmailAddress,
    emailUnvalidated :: Maybe EmailAddress,
    expires :: Maybe UTCTimeMillis,
    featureConferenceCalling :: Maybe Int32,
    handle :: Maybe Handle,
    language :: Maybe Language,
    managedBy :: Maybe ManagedBy,
    name :: Maybe Name,
    password :: Maybe Password,
    providerId :: Maybe ProviderId,
    searchable :: Maybe Bool,
    serviceId :: Maybe ServiceId,
    ssoId :: Maybe UserSSOId,
    status :: Maybe AccountStatus,
    supportedProtocols :: Maybe (Set BaseProtocolTag),
    teamId :: Maybe TeamId,
    textStatus :: Maybe TextStatus,
    userType :: Maybe UserType,
    assets :: Maybe [Asset],
    pict :: Maybe Pict,
    activatedWriteTime :: Maybe (Writetime ())
  }

data ServiceConv = ServiceConv
  { convId :: ConvId,
    teamId :: Maybe TeamId
  }

data UserRowPG = UserRowPG
  { id_ :: UserId,
    accentId :: ColourId,
    activated :: Bool,
    country :: Maybe Country,
    email :: Maybe EmailAddress,
    emailUnvalidated :: Maybe EmailAddress,
    expires :: Maybe UTCTimeMillis,
    featureConferenceCalling :: Maybe Int32,
    handle :: Maybe Handle,
    language :: Maybe Language,
    managedBy :: Maybe ManagedBy,
    name :: Name,
    password :: Maybe Password,
    providerId :: Maybe ProviderId,
    searchable :: Maybe Bool,
    serviceId :: Maybe ServiceId,
    ssoId :: Maybe UserSSOId,
    status :: Maybe AccountStatus,
    supportedProtocols :: Maybe (Set BaseProtocolTag),
    teamId :: Maybe TeamId,
    textStatus :: Maybe TextStatus,
    userType :: UserType,
    assets :: Maybe [Asset],
    pict :: Maybe Pict,
    richInfo :: Maybe RichInfoAssocList,
    createdAt :: UTCTime
  }

recordInstance ''UserRowCass

recordInstance ''ServiceConv
