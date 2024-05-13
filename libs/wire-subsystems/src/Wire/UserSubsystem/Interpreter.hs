module Wire.UserSubsystem.Interpreter
  ( runUserSubsystem,
    UserSubsystemConfig (..),
  )
where

import Control.Lens (view)
import Control.Monad.Trans.Maybe
import Data.Either.Extra
import Data.Id
import Data.Json.Util
import Data.LegalHold
import Data.Qualified
import Data.Time.Clock
import Imports hiding (local)
import Polysemy
import Polysemy.Error hiding (try)
import Polysemy.Input
import Servant.Client.Core
import Wire.API.Federation.API
import Wire.API.Federation.Error
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.User
import Wire.API.UserEvent (profileUpdated)
import Wire.Arbitrary
import Wire.DeleteQueue
import Wire.FederationAPIAccess
import Wire.GalleyAPIAccess
import Wire.Sem.Concurrency
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.StoredUser
import Wire.UserEvents
import Wire.UserStore
import Wire.UserSubsystem (AllowSCIMUpdates (..), UserSubsystem (..), UserSubsystemError (..))

data UserSubsystemConfig = UserSubsystemConfig
  { emailVisibilityConfig :: EmailVisibilityConfig,
    defaultLocale :: Locale
  }
  deriving (Show)

instance Arbitrary UserSubsystemConfig where
  arbitrary = UserSubsystemConfig <$> arbitrary <*> arbitrary

runUserSubsystem ::
  ( Member GalleyAPIAccess r,
    Member UserStore r,
    Member (Concurrency 'Unsafe) r, -- FUTUREWORK: subsystems should implement concurrency inside interpreters, not depend on this dangerous effect.
    Member (Error FederationError) r,
    Member (Error UserSubsystemError) r,
    Member (FederationAPIAccess fedM) r,
    Member DeleteQueue r,
    Member UserEvents r,
    Member Now r,
    RunClient (fedM 'Brig),
    FederationMonad fedM,
    Typeable fedM
  ) =>
  UserSubsystemConfig ->
  InterpreterFor UserSubsystem r
runUserSubsystem cfg = runInputConst cfg . interpretUserSubsystem . raiseUnder

interpretUserSubsystem ::
  ( Member GalleyAPIAccess r,
    Member UserStore r,
    Member (Concurrency 'Unsafe) r,
    Member (Error FederationError) r,
    Member (Error UserSubsystemError) r,
    Member (FederationAPIAccess fedM) r,
    Member (Input UserSubsystemConfig) r,
    Member DeleteQueue r,
    Member UserEvents r,
    Member Now r,
    RunClient (fedM 'Brig),
    FederationMonad fedM,
    Typeable fedM
  ) =>
  InterpreterFor UserSubsystem r
interpretUserSubsystem = interpret $ \case
  GetUserProfiles self others -> getUserProfilesImpl self others
  GetLocalUserProfiles others -> getLocalUserProfilesImpl others
  GetUserProfilesWithErrors self others -> getUserProfilesWithErrorsImpl self others
  UpdateUserProfile self mconn update allowScim -> updateUserProfileImpl self mconn update allowScim

-- | Obtain user profiles for a list of users as they can be seen by
-- a given user 'self'. If 'self' is an unknown 'UserId', return '[]'.
getUserProfilesImpl ::
  ( Member GalleyAPIAccess r,
    Member (Input UserSubsystemConfig) r,
    Member UserStore r,
    Member (Concurrency 'Unsafe) r, -- FUTUREWORK: subsystems should implement concurrency inside interpreters, not depend on this dangerous effect.
    Member (Error FederationError) r,
    Member (FederationAPIAccess fedM) r,
    Member DeleteQueue r,
    Member Now r,
    RunClient (fedM 'Brig),
    FederationMonad fedM,
    Typeable fedM
  ) =>
  -- | User 'self' on whose behalf the profiles are requested.
  Local UserId ->
  -- | The users ('others') for which to obtain the profiles.
  [Qualified UserId] ->
  Sem r [UserProfile]
getUserProfilesImpl self others =
  concat
    <$> unsafePooledMapConcurrentlyN
      8
      (getUserProfilesFromDomain self)
      (bucketQualified others)

getLocalUserProfilesImpl ::
  forall r.
  ( Member UserStore r,
    Member (Input UserSubsystemConfig) r,
    Member DeleteQueue r,
    Member Now r,
    Member GalleyAPIAccess r
  ) =>
  Local [UserId] ->
  Sem r [UserProfile]
getLocalUserProfilesImpl = getUserProfilesLocalPart Nothing

getUserProfilesFromDomain ::
  ( Member GalleyAPIAccess r,
    Member (Error FederationError) r,
    Member (Input UserSubsystemConfig) r,
    Member (FederationAPIAccess fedM) r,
    Member DeleteQueue r,
    Member Now r,
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
    (getUserProfilesLocalPart (Just self))
    getUserProfilesRemotePart

getUserProfilesRemotePart ::
  ( Member (FederationAPIAccess fedM) r,
    Member (Error FederationError) r,
    RunClient (fedM 'Brig),
    FederationMonad fedM,
    Typeable fedM
  ) =>
  Remote [UserId] ->
  Sem r [UserProfile]
getUserProfilesRemotePart ruids = do
  runFederated ruids $ fedClient @'Brig @"get-users-by-ids" (tUnqualified ruids)

getUserProfilesLocalPart ::
  forall r.
  ( Member UserStore r,
    Member (Input UserSubsystemConfig) r,
    Member DeleteQueue r,
    Member Now r,
    Member GalleyAPIAccess r
  ) =>
  Maybe (Local UserId) ->
  Local [UserId] ->
  Sem r [UserProfile]
getUserProfilesLocalPart requestingUser luids = do
  emailVisibilityConfig <- inputs emailVisibilityConfig
  emailVisibilityConfigWithViewer <-
    case emailVisibilityConfig of
      EmailVisibleIfOnTeam -> pure EmailVisibleIfOnTeam
      EmailVisibleToSelf -> pure EmailVisibleToSelf
      EmailVisibleIfOnSameTeam () ->
        EmailVisibleIfOnSameTeam . join @Maybe
          <$> traverse getRequestingUserInfo requestingUser
  -- FUTUREWORK: (in the interpreters where it makes sense) pull paginated lists from the DB,
  -- not just single rows.
  catMaybes <$> traverse (getLocalUserProfile emailVisibilityConfigWithViewer) (sequence luids)
  where
    getRequestingUserInfo :: Local UserId -> Sem r (Maybe (TeamId, TeamMember))
    getRequestingUserInfo self = do
      -- FUTUREWORK: it is an internal error for the two lookups (for 'User' and 'TeamMember')
      -- to return 'Nothing'.  we could throw errors here if that happens, rather than just
      -- returning an empty profile list from 'lookupProfiles'.
      mUser <- getUser $ tUnqualified self
      let mUserNotPending = do
            user <- mUser
            guard $ not (hasPendingInvitation user)
            pure user
      case mUserNotPending >>= (.teamId) of
        Nothing -> pure Nothing
        Just tid -> (tid,) <$$> getTeamMember (tUnqualified self) tid

getLocalUserProfile ::
  forall r.
  ( Member UserStore r,
    Member GalleyAPIAccess r,
    Member DeleteQueue r,
    Member Now r,
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
    lhs :: UserLegalHoldStatus <- do
      teamMember <- lift $ join <$> (getTeamMember storedUser.id `mapM` storedUser.teamId)
      pure $ maybe defUserLegalHoldStatus (view legalHoldStatus) teamMember
    let user = mkUserFromStored domain locale storedUser
        usrProfile = mkUserProfile emailVisibilityConfigWithViewer user lhs
    lift $ deleteLocalIfExpired user
    pure usrProfile

-- | ephemeral users past their expiry date are queued for deletion
deleteLocalIfExpired :: forall r. (Member DeleteQueue r, Member Now r) => User -> Sem r ()
deleteLocalIfExpired user =
  case user.userExpire of
    Nothing -> pure ()
    Just (fromUTCTimeMillis -> e) -> do
      t <- Now.get
      when (diffUTCTime e t < 0) $
        enqueueUserDeletion (qUnqualified user.userQualifiedId)

getUserProfilesWithErrorsImpl ::
  forall r fedM.
  ( Member UserStore r,
    Member (Concurrency 'Unsafe) r, -- FUTUREWORK: subsystems should implement concurrency inside interpreters, not depend on this dangerous effect.
    Member (Input UserSubsystemConfig) r,
    Member (FederationAPIAccess fedM) r,
    Member GalleyAPIAccess r,
    Member DeleteQueue r,
    Member Now r,
    RunClient (fedM 'Brig),
    FederationMonad fedM,
    Typeable fedM
  ) =>
  Local UserId ->
  [Qualified UserId] ->
  Sem r ([(Qualified UserId, FederationError)], [UserProfile])
getUserProfilesWithErrorsImpl self others = do
  aggregate ([], []) <$> unsafePooledMapConcurrentlyN 8 go (bucketQualified others)
  where
    go :: Qualified [UserId] -> Sem r (Either (FederationError, Qualified [UserId]) [UserProfile])
    go bucket = runError (getUserProfilesFromDomain self bucket) <&> mapLeft (,bucket)
    -- this function will partition the Eithers into a list of pairs such that
    -- - the left side will contain a list of users with a federation error 'Left's
    -- - the right side will contain a list of user profiles obtained from the 'Right's
    -- - the left side will have to transform a pair of error and user ids into a list
    --   of users ids paired with errors; this is done by just pairing all of them with
    --   the same error
    aggregate ::
      ( inp ~ [Either (FederationError, Qualified [UserId]) [UserProfile]],
        outp ~ ([(Qualified UserId, FederationError)], [UserProfile])
      ) =>
      (outp -> inp -> outp)
    aggregate acc [] = acc
    aggregate (accL, accR) (Right prof : buckets) = aggregate (accL, prof <> accR) buckets
    aggregate (accL, accR) (Left err : buckets) = aggregate (renderBucketError err <> accL, accR) buckets

    renderBucketError :: (FederationError, Qualified [UserId]) -> [(Qualified UserId, FederationError)]
    renderBucketError (err, qlist) = (,err) . (flip Qualified (qDomain qlist)) <$> qUnqualified qlist

updateUserProfileImpl ::
  ( Member UserStore r,
    Member (Error UserSubsystemError) r,
    Member UserEvents r,
    Member GalleyAPIAccess r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  UserUpdate ->
  AllowSCIMUpdates ->
  Sem r ()
updateUserProfileImpl (tUnqualified -> uid) mconn update allowScim = do
  -- check if name updates are allowed
  for_ (uupName update) $ \newName -> do
    mbUser <- getUser uid
    user <- maybe (throw UserSubsystemProfileNotFound) pure mbUser
    unless
      ( user.managedBy /= Just ManagedByScim
          || user.name == newName
          || allowScim == AllowSCIMUpdates
      )
      $ throw UserSubsystemDisplayNameManagedByScim
    hasE2EId <-
      wsStatus . afcMlsE2EId <$> getAllFeatureConfigsForUser (Just uid) <&> \case
        FeatureStatusEnabled -> True
        FeatureStatusDisabled -> False
    when (hasE2EId && newName /= user.name) $
      throw UserSubsystemDisplayNameManagedByScim

  updateUser uid update
  generateUserEvent uid mconn (profileUpdated uid update)
