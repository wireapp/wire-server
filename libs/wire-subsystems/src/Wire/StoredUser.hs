{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -Wno-ambiguous-fields #-}
{-# OPTIONS -Wno-orphans #-}

module Wire.StoredUser where

import Data.Domain
import Data.Handle
import Data.Id
import Data.Json.Util
import Data.Qualified
import Data.Set qualified as S
import Database.CQL.Protocol (Record (..), TupleType, recordInstance)
import GHC.Records
import Imports
import Wire.API.Locale
import Wire.API.Password
import Wire.API.Provider.Service
import Wire.API.User
import Wire.Arbitrary

data StoredUser = StoredUser
  { id :: UserId,
    name :: Name,
    textStatus :: Maybe TextStatus,
    pict :: Maybe Pict,
    email :: Maybe EmailAddress,
    emailUnvalidated :: Maybe EmailAddress,
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
  deriving (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform StoredUser)

recordInstance ''StoredUser

setStoredUserName :: Name -> StoredUser -> StoredUser
setStoredUserName newName user = user {name = newName}

setStoredUserSupportedProtocols :: Set BaseProtocolTag -> StoredUser -> StoredUser
setStoredUserSupportedProtocols newProtocols user = user {supportedProtocols = Just newProtocols}

setStoredUserPict :: Pict -> StoredUser -> StoredUser
setStoredUserPict newPict user = user {pict = Just newPict}

setStoredUserAssets :: [Asset] -> StoredUser -> StoredUser
setStoredUserAssets newAssets user = user {assets = Just newAssets}

setStoredUserAccentId :: ColourId -> StoredUser -> StoredUser
setStoredUserAccentId newAccentId user = user {accentId = newAccentId}

setStoredUserLocale :: Locale -> StoredUser -> StoredUser
setStoredUserLocale newLocale user =
  user
    { language = Just newLocale.lLanguage,
      country = newLocale.lCountry
    }

setStoredUserHandle :: Handle -> StoredUser -> StoredUser
setStoredUserHandle newHandle user = user {handle = Just newHandle}

hasPendingInvitation :: StoredUser -> Bool
hasPendingInvitation u = u.status == Just PendingInvitation

mkUserFromStored :: Domain -> Locale -> StoredUser -> User
mkUserFromStored domain defaultLocale storedUser =
  let expiration = if storedUser.status == Just Ephemeral then storedUser.expires else Nothing
      loc = toLocale defaultLocale (storedUser.language, storedUser.country)
      svc = newServiceRef <$> storedUser.serviceId <*> storedUser.providerId
   in User
        { userQualifiedId = (Qualified storedUser.id domain),
          userIdentity = storedUser.identity,
          userEmailUnvalidated = storedUser.emailUnvalidated,
          userDisplayName = storedUser.name,
          userTextStatus = storedUser.textStatus,
          userPict = (fromMaybe noPict storedUser.pict),
          userAssets = (fromMaybe [] storedUser.assets),
          userAccentId = storedUser.accentId,
          userStatus = fromMaybe Active storedUser.status,
          userLocale = loc,
          userService = svc,
          userHandle = storedUser.handle,
          userExpire = expiration,
          userTeam = storedUser.teamId,
          userManagedBy = fromMaybe ManagedByWire storedUser.managedBy,
          userSupportedProtocols = case storedUser.supportedProtocols of
            Nothing -> defSupportedProtocols
            Just ps -> if S.null ps then defSupportedProtocols else ps
        }

toLocale :: Locale -> (Maybe Language, Maybe Country) -> Locale
toLocale _ (Just l, c) = Locale l c
toLocale l _ = l

-- | If the user is not activated, 'toIdentity' will return 'Nothing' as a
-- precaution, because elsewhere we rely on the fact that a non-empty
-- 'UserIdentity' means that the user is activated.
--
-- The reason it's just a "precaution" is that we /also/ have an invariant that
-- having an email or phone in the database means the user has to be activated.
toIdentity ::
  -- | Whether the user is activated
  Bool ->
  Maybe EmailAddress ->
  Maybe UserSSOId ->
  Maybe UserIdentity
toIdentity True (Just e) Nothing = Just $! EmailIdentity e
toIdentity True email (Just ssoid) = Just $! SSOIdentity ssoid email
toIdentity True Nothing Nothing = Nothing
toIdentity False _ _ = Nothing

instance HasField "identity" StoredUser (Maybe UserIdentity) where
  getField user = toIdentity user.activated user.email user.ssoId

instance HasField "locale" StoredUser (Maybe Locale) where
  getField user = Locale <$> user.language <*> pure user.country

--------------------------------------------------------------------------------

data NewStoredUser = NewStoredUser
  { id :: UserId,
    name :: Name,
    textStatus :: Maybe TextStatus,
    pict :: Pict,
    assets :: [Asset],
    email :: Maybe EmailAddress,
    ssoId :: Maybe UserSSOId,
    accentId :: ColourId,
    password :: Maybe Password,
    activated :: Bool,
    status :: AccountStatus,
    expires :: Maybe UTCTimeMillis,
    language :: Language,
    country :: Maybe Country,
    providerId :: Maybe ProviderId,
    serviceId :: Maybe ServiceId,
    handle :: Maybe Handle,
    teamId :: Maybe TeamId,
    managedBy :: ManagedBy,
    supportedProtocols :: Set BaseProtocolTag
  }
  deriving (Show)

recordInstance ''NewStoredUser

-- addPrepQuery requires a Show instance for the input tuple type, but Show
-- instances are only defined up to 15 elements. Note that we can't use the
-- TupleType type family because type families in instance declarations are not
-- allowed.
deriving instance
  Show
    ( UserId,
      Name,
      Maybe TextStatus,
      Pict,
      [Asset],
      Maybe EmailAddress,
      Maybe UserSSOId,
      ColourId,
      Maybe Password,
      Bool,
      AccountStatus,
      Maybe UTCTimeMillis,
      Language,
      Maybe Country,
      Maybe ProviderId,
      Maybe ServiceId,
      Maybe Handle,
      Maybe TeamId,
      ManagedBy,
      Set BaseProtocolTag
    )

instance HasField "service" NewStoredUser (Maybe ServiceRef) where
  getField user = ServiceRef <$> user.serviceId <*> user.providerId

newStoredUserToUser :: Qualified NewStoredUser -> User
newStoredUserToUser (Qualified new domain) =
  User
    { userQualifiedId = Qualified new.id domain,
      userIdentity = toIdentity True new.email new.ssoId,
      userEmailUnvalidated = Nothing,
      userDisplayName = new.name,
      userTextStatus = new.textStatus,
      userPict = new.pict,
      userAssets = new.assets,
      userAccentId = new.accentId,
      userStatus = new.status,
      userLocale = Locale new.language new.country,
      userService = newServiceRef <$> new.serviceId <*> new.providerId,
      userHandle = new.handle,
      userExpire = new.expires,
      userTeam = new.teamId,
      userManagedBy = new.managedBy,
      userSupportedProtocols = new.supportedProtocols
    }
