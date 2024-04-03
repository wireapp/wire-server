{-# LANGUAGE TemplateHaskell #-}

module Wire.UserStore where

import Data.Handle
import Data.Id
import Data.Json.Util
import Imports
import Polysemy
import Wire.API.User
import Wire.Arbitrary

data StoredUser = StoredUser
  { id_ :: UserId,
    name :: Name,
    pict :: Maybe Pict,
    email :: Maybe Email,
    phone :: Maybe Phone,
    ssoId :: Maybe UserSSOId,
    accentId :: ColourId,
    assets :: Maybe [Asset],
    activated :: Bool,
    status :: Maybe AccountStatus,
    expires :: Maybe UTCTimeMillis,
    language :: Maybe Language,
    country :: Maybe Country,
    providerId :: Maybe ProviderId,
    serviceId :: Maybe ServiceId,
    handle :: Maybe Handle,
    teamId :: Maybe TeamId,
    managedBy :: Maybe ManagedBy,
    supportedProtocols :: Maybe (Set BaseProtocolTag)
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform StoredUser)

data UserStore m a where
  GetUser :: UserId -> UserStore m (Maybe StoredUser)

makeSem ''UserStore
