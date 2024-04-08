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
import Servant.Client.Core
import Wire.API.Federation.API
import Wire.API.Federation.Error
import Wire.API.Team.Member
import Wire.API.User
import Wire.FederationAPIAccess
import Wire.GalleyAPIAccess
import Wire.Sem.Concurrency
import Wire.StoredUser
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
    Member (FederationAPIAccess fedM) r,
    Member (Input UserSubsystemConfig) r,
    Member UserStore r,
    RunClient (fedM 'Brig),
    FederationMonad fedM
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
  ( Member GalleyAPIAccess r,
    Member (Error FederationError) r,
    Member (Input UserSubsystemConfig) r,
    Member (FederationAPIAccess fedM) r,
    Member UserStore r,
    RunClient (fedM 'Brig),
    FederationMonad fedM
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
  ( Member (FederationAPIAccess fedM) r,
    Member (Error FederationError) r,
    RunClient (fedM 'Brig),
    FederationMonad fedM
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
