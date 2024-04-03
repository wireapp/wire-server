{-# OPTIONS_GHC -Wwarn #-}

module Wire.UserSubsystem.Interpreter where

import Control.Monad.Trans.Maybe
import Data.Domain
import Data.Id
import Data.LegalHold
import Data.Qualified
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Federation.API
import Wire.API.Federation.Error
import Wire.API.Provider.Service
import Wire.API.Team.Member
import Wire.API.User
import Wire.FederationAPIAccess
import Wire.GalleyAPIAccess
import Wire.Sem.Concurrency
import Wire.UserStore

data UserSubsystemConfig = UserSubsystemConfig
  { emailVisibilityConfig :: EmailVisibilityConfig,
    defaultDomain :: Domain,
    defaultLocale :: Locale
  }

-- | Obtain user profiles for a list of users as they can be seen by
-- a given user 'self'. If 'self' is an unknown 'UserId', return '[]'.
getUserProfiles ::
  ( Member GalleyAPIAccess r,
    Member (Concurrency 'Unsafe) r,
    Member (Error FederationError) r,
    Member FederationAPIAccess r,
    Member (Input UserSubsystemConfig) r,
    Member UserStore r
  ) =>
  -- | User 'self' on whose behalf the profiles are requested.
  Local UserId ->
  -- | The users ('others') for which to obtain the profiles.
  [Qualified UserId] ->
  Sem r [UserProfile]
getUserProfiles self others =
  concat
    <$> unsafePooledMapConcurrentlyN
      8
      (getUserProfilesFromDomain self)
      (bucketQualified others)

getUserProfilesFromDomain ::
  ( Member (Concurrency 'Unsafe) r,
    Member GalleyAPIAccess r,
    Member (Error FederationError) r,
    Member (Input UserSubsystemConfig) r,
    Member FederationAPIAccess r,
    Member UserStore r
  ) =>
  Local UserId ->
  Qualified [UserId] ->
  Sem r [UserProfile]
getUserProfilesFromDomain self =
  foldQualified
    self
    (getLocalUserProfiles (tUnqualified self) . tUnqualified)
    getRemoteUserProfiles

getRemoteUserProfiles ::
  ( Member FederationAPIAccess r,
    Member (Concurrency 'Unsafe) r,
    Member (Error FederationError) r
  ) =>
  Remote [UserId] ->
  Sem r [UserProfile]
getRemoteUserProfiles ruids = do
  runFederated ruids $ fedClient @'Brig @"get-users-by-ids" (tUnqualified ruids)

getLocalUserProfiles ::
  forall r.
  ( Member UserStore r,
    Member (Input UserSubsystemConfig) r,
    Member GalleyAPIAccess r
  ) =>
  UserId ->
  [UserId] ->
  Sem r [UserProfile]
getLocalUserProfiles requestingUser uids = do
  emailVisibility <- inputs emailVisibilityConfig
  emailVisibilityConfigWithViewer <-
    traverse
      (const (getSelfInfo requestingUser))
      emailVisibility
  catMaybes <$> traverse (getLocalUserProfile emailVisibilityConfigWithViewer) uids
  where
    getSelfInfo :: UserId -> Sem r (Maybe (TeamId, TeamMember))
    getSelfInfo selfId = do
      -- FUTUREWORK: it is an internal error for the two lookups (for 'User' and 'TeamMember')
      -- to return 'Nothing'.  we could throw errors here if that happens, rather than just
      -- returning an empty profile list from 'lookupProfiles'.
      mUser <- getUser selfId
      let mUserNotPending = do
            user <- mUser
            guard $ not (hasPendingInvitation user)
            pure user
      case mUserNotPending >>= (.teamId) of
        Nothing -> pure Nothing
        Just tid -> (tid,) <$$> getTeamMember selfId tid

getLocalUserProfile ::
  forall r.
  ( Member UserStore r,
    Member (Input UserSubsystemConfig) r
  ) =>
  EmailVisibilityConfigWithViewer ->
  UserId ->
  Sem r (Maybe UserProfile)
getLocalUserProfile emailVisibilityConfigWithViewer uid = do
  domain <- inputs defaultDomain
  locale <- inputs defaultLocale
  runMaybeT $ do
    storedUser <- MaybeT $ getUser uid
    guard $ not (hasPendingInvitation storedUser)
    let user = mkUserFromStored domain locale storedUser
    pure $ mkUserProfile emailVisibilityConfigWithViewer user UserLegalHoldDisabled

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
        { userQualifiedId = (Qualified storedUser.id_ domain),
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
