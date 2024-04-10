module Wire.StoredUser where

import Data.Domain
import Data.Handle
import Data.Id
import Data.Json.Util
import Data.Qualified
import Imports
import Wire.API.Provider.Service
import Wire.API.User
import Wire.Arbitrary

data StoredUser = StoredUser
  { id :: UserId,
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
  deriving (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via (GenericUniform StoredUser)

hasPendingInvitation :: StoredUser -> Bool
hasPendingInvitation u = u.status == Just PendingInvitation

mkUserFromStored :: Domain -> Locale -> StoredUser -> User
mkUserFromStored domain defaultLocale storedUser =
  let ident = toIdentity storedUser.activated storedUser.email storedUser.phone storedUser.ssoId
      deleted = Just Deleted == storedUser.status
      expiration = if storedUser.status == Just Ephemeral then storedUser.expires else Nothing
      loc = toLocale defaultLocale (storedUser.language, storedUser.country)
      svc = newServiceRef <$> storedUser.serviceId <*> storedUser.providerId
   in User
        { userQualifiedId = (Qualified storedUser.id domain),
          userIdentity = ident,
          userDisplayName = storedUser.name,
          userPict = (fromMaybe noPict storedUser.pict),
          userAssets = (fromMaybe [] storedUser.assets),
          userAccentId = storedUser.accentId,
          userDeleted = deleted,
          userLocale = loc,
          userService = svc,
          userHandle = storedUser.handle,
          userExpire = expiration,
          userTeam = storedUser.teamId,
          userManagedBy = (fromMaybe ManagedByWire storedUser.managedBy),
          userSupportedProtocols = (fromMaybe defSupportedProtocols storedUser.supportedProtocols)
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
  Maybe Email ->
  Maybe Phone ->
  Maybe UserSSOId ->
  Maybe UserIdentity
toIdentity True (Just e) (Just p) Nothing = Just $! FullIdentity e p
toIdentity True (Just e) Nothing Nothing = Just $! EmailIdentity e
toIdentity True Nothing (Just p) Nothing = Just $! PhoneIdentity p
toIdentity True email phone (Just ssoid) = Just $! SSOIdentity ssoid email phone
toIdentity True Nothing Nothing Nothing = Nothing
toIdentity False _ _ _ = Nothing
