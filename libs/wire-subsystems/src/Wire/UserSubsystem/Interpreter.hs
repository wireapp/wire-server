module Wire.UserSubsystem.Interpreter where

import Control.Monad.Trans.Maybe
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
import Wire.UserSubsystem (UserSubsystem (..))

data UserSubsystemConfig = UserSubsystemConfig
  { emailVisibilityConfig :: EmailVisibilityConfig,
    defaultLocale :: Locale
  }

runUserSubsystem ::
  ( Member GalleyAPIAccess r,
    Member UserStore r,
    Member (Concurrency 'Unsafe) r,
    Member (Error FederationError) r,
    Member (FederationAPIAccess fedM) r,
    RunClient (fedM 'Brig),
    FederationMonad fedM,
    Typeable fedM
  ) =>
  UserSubsystemConfig ->
  Sem (UserSubsystem : r) a ->
  Sem r a
runUserSubsystem cfg = interpret $ \case
  GetUserProfile self other -> runInputConst cfg $ getUserProfile self other
  GetUserProfiles self others -> runInputConst cfg $ getUserProfiles self others

getUserProfile ::
  ( Member GalleyAPIAccess r,
    Member (Input UserSubsystemConfig) r,
    Member UserStore r,
    Member (Concurrency 'Unsafe) r,
    Member (Error FederationError) r,
    Member (FederationAPIAccess fedM) r,
    RunClient (fedM 'Brig),
    FederationMonad fedM,
    Typeable fedM
  ) =>
  Local UserId ->
  Qualified UserId ->
  Sem r (Maybe UserProfile)
getUserProfile self other = listToMaybe <$> getUserProfiles self [other]

-- | Obtain user profiles for a list of users as they can be seen by
-- a given user 'self'. If 'self' is an unknown 'UserId', return '[]'.
getUserProfiles ::
  ( Member GalleyAPIAccess r,
    Member (Input UserSubsystemConfig) r,
    Member UserStore r,
    Member (Concurrency 'Unsafe) r,
    Member (Error FederationError) r,
    Member (FederationAPIAccess fedM) r,
    RunClient (fedM 'Brig),
    FederationMonad fedM,
    Typeable fedM
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
    FederationMonad fedM,
    Typeable fedM
  ) =>
  Local UserId ->
  Qualified [UserId] ->
  Sem r [UserProfile]
getUserProfilesFromDomain self =
  foldQualified
    self
    (getLocalUserProfiles self)
    getRemoteUserProfiles

getRemoteUserProfiles ::
  ( Member (FederationAPIAccess fedM) r,
    Member (Error FederationError) r,
    RunClient (fedM 'Brig),
    FederationMonad fedM,
    Typeable fedM
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
  Local UserId ->
  Local [UserId] ->
  Sem r [UserProfile]
getLocalUserProfiles requestingUser luids = do
  emailVisibility <- inputs emailVisibilityConfig
  emailVisibilityConfigWithViewer <-
    traverse (const getRequestingUserInfo) emailVisibility
  -- FUTUREWORK: Does it make sense to pull these parallely?
  catMaybes <$> traverse (getLocalUserProfile emailVisibilityConfigWithViewer) (sequence luids)
  where
    getRequestingUserInfo :: Sem r (Maybe (TeamId, TeamMember))
    getRequestingUserInfo = do
      -- FUTUREWORK: it is an internal error for the two lookups (for 'User' and 'TeamMember')
      -- to return 'Nothing'.  we could throw errors here if that happens, rather than just
      -- returning an empty profile list from 'lookupProfiles'.
      mUser <- getUser $ tUnqualified requestingUser
      let mUserNotPending = do
            user <- mUser
            guard $ not (hasPendingInvitation user)
            pure user
      case mUserNotPending >>= (.teamId) of
        Nothing -> pure Nothing
        Just tid -> (tid,) <$$> getTeamMember (tUnqualified requestingUser) tid

getLocalUserProfile ::
  forall r.
  ( Member UserStore r,
    Member DeleteQueueEffect r,
    Member (Input UserSubsystemConfig) r
  ) =>
  EmailVisibilityConfigWithViewer ->
  Local UserId ->
  Sem r (Maybe UserProfile)
getLocalUserProfile emailVisibilityConfigWithViewer luid = do
  let domain = tDomain luid
  locale <- inputs defaultLocale
  runMaybeT $ do
    storedUser <- MaybeT $ getUser (tUnqualified luid)
    guard $ not (hasPendingInvitation storedUser)
    let user = mkUserFromStored domain locale storedUser >>= mkUserProfile emailVisibilityConfigWithViewer user UserLegalHoldDisabled
    pure _
    -- TODO: Garbage collect expired users
    -- case user.userExpire of
    --   Nothing -> pure user
    --   Just (fromUTCTimeMillis -> e) -> do
    --     now <- liftIO =<< view currentTime
    --     -- ephemeral users past their expiry date are deleted
    --     when (diffUTCTime e now < 0) $
    --       enqueueUserDeletion user.userId
    --     pure user
