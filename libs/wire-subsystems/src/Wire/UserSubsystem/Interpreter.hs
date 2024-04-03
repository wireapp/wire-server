{-# OPTIONS_GHC -Wwarn #-}

module Wire.UserSubsystem.Interpreter where

import Control.Monad.Trans.Maybe
import Data.Domain
import Data.Id
import Data.LegalHold
import Data.Qualified
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Provider.Service
import Wire.API.Team.Member
import Wire.API.User
import Wire.GalleyAPIAccess
import Wire.UserStore

data UserSubsystemConfig = UserSubsystemConfig
  { emailVisibilityConfig :: EmailVisibilityConfig,
    defaultDomain :: Domain,
    defaultLocale :: Locale
  }

getRemoteUserProfile :: UserId -> Remote UserId -> Sem r (Maybe UserProfile)
getRemoteUserProfile = undefined

getLocalUserProfile ::
  forall r.
  ( Member UserStore r,
    Member (Input UserSubsystemConfig) r,
    Member GalleyAPIAccess r
  ) =>
  UserId ->
  UserId ->
  Sem r (Maybe UserProfile)
getLocalUserProfile requestingUser uid = do
  emailVisibilityConfig <- inputs emailVisibilityConfig
  emailVisibilityConfigWithViewer <-
    traverse
      (const (getSelfInfo requestingUser))
      emailVisibilityConfig
  domain <- inputs defaultDomain
  locale <- inputs defaultLocale
  user <- runMaybeT $ do
    u <- MaybeT $ getUser uid
    guard $ not (hasPendingInvitation u)
    pure $ mkUserFromStored domain locale u
  pure $ (\u -> mkUserProfile emailVisibilityConfigWithViewer u UserLegalHoldDisabled) <$> user
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
