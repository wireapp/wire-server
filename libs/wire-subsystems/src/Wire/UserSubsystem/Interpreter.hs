{-# LANGUAGE RecordWildCards #-}

module Wire.UserSubsystem.Interpreter
  ( runUserSubsystem,
    UserSubsystemConfig (..),
  )
where

import Control.Lens (view)
import Control.Monad.Trans.Maybe
import Data.Handle (Handle)
import Data.Handle qualified as Handle
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
import Wire.API.Team.Member hiding (userId)
import Wire.API.User
import Wire.API.UserEvent
import Wire.Arbitrary
import Wire.BlockListStore as BlockList
import Wire.DeleteQueue
import Wire.Events
import Wire.FederationAPIAccess
import Wire.GalleyAPIAccess
import Wire.Sem.Concurrency
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.StoredUser
import Wire.UserKeyStore
import Wire.UserStore as UserStore
import Wire.UserSubsystem
import Wire.UserSubsystem.Error
import Wire.UserSubsystem.HandleBlacklist

data UserSubsystemConfig = UserSubsystemConfig
  { emailVisibilityConfig :: EmailVisibilityConfig,
    defaultLocale :: Locale,
    searchSameTeamOnly :: Bool
  }
  deriving (Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserSubsystemConfig)

runUserSubsystem ::
  ( Member GalleyAPIAccess r,
    Member UserStore r,
    Member UserKeyStore r,
    Member BlockListStore r,
    Member (Concurrency 'Unsafe) r, -- FUTUREWORK: subsystems should implement concurrency inside interpreters, not depend on this dangerous effect.
    Member (Error FederationError) r,
    Member (Error UserSubsystemError) r,
    Member (FederationAPIAccess fedM) r,
    Member DeleteQueue r,
    Member Events r,
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
    Member UserKeyStore r,
    Member BlockListStore r,
    Member (Concurrency 'Unsafe) r,
    Member (Error FederationError) r,
    Member (Error UserSubsystemError) r,
    Member (FederationAPIAccess fedM) r,
    Member (Input UserSubsystemConfig) r,
    Member DeleteQueue r,
    Member Events r,
    Member Now r,
    RunClient (fedM 'Brig),
    FederationMonad fedM,
    Typeable fedM
  ) =>
  InterpreterFor UserSubsystem r
interpretUserSubsystem = interpret \case
  GetUserProfiles self others -> getUserProfilesImpl self others
  GetLocalUserProfiles others -> getLocalUserProfilesImpl others
  GetSelfProfile self -> getSelfProfileImpl self
  GetUserProfilesWithErrors self others -> getUserProfilesWithErrorsImpl self others
  UpdateUserProfile self mconn mb update -> updateUserProfileImpl self mconn mb update
  CheckHandle uhandle -> checkHandleImpl uhandle
  CheckHandles hdls cnt -> checkHandlesImpl hdls cnt
  UpdateHandle uid mconn mb uhandle -> updateHandleImpl uid mconn mb uhandle
  GetLocalUserAccountByUserKey userKey -> getLocalUserAccountByUserKeyImpl userKey
  LookupLocaleWithDefault luid -> lookupLocaleOrDefaultImpl luid
  IsBlocked email -> isBlockedImpl email
  BlockListDelete email -> blockListDeleteImpl email
  BlockListInsert email -> blockListInsertImpl email

isBlockedImpl :: (Member BlockListStore r) => EmailAddress -> Sem r Bool
isBlockedImpl = BlockList.exists . mkEmailKey

blockListDeleteImpl :: (Member BlockListStore r) => EmailAddress -> Sem r ()
blockListDeleteImpl = BlockList.delete . mkEmailKey

blockListInsertImpl :: (Member BlockListStore r) => EmailAddress -> Sem r ()
blockListInsertImpl = BlockList.insert . mkEmailKey

lookupLocaleOrDefaultImpl :: (Member UserStore r, Member (Input UserSubsystemConfig) r) => Local UserId -> Sem r (Maybe Locale)
lookupLocaleOrDefaultImpl luid = do
  mLangCountry <- UserStore.lookupLocale (tUnqualified luid)
  defLocale <- inputs defaultLocale
  pure (toLocale defLocale <$> mLangCountry)

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
    Member GalleyAPIAccess r,
    Member (Concurrency Unsafe) r
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
    Typeable fedM,
    Member (Concurrency Unsafe) r
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
    Member GalleyAPIAccess r,
    Member (Concurrency Unsafe) r
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
  catMaybes <$> unsafePooledForConcurrentlyN 8 (sequence luids) (getLocalUserProfileImpl emailVisibilityConfigWithViewer)
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

getLocalUserProfileImpl ::
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
getLocalUserProfileImpl emailVisibilityConfigWithViewer luid = do
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

getSelfProfileImpl ::
  ( Member (Input UserSubsystemConfig) r,
    Member UserStore r,
    Member GalleyAPIAccess r
  ) =>
  Local UserId ->
  Sem r (Maybe SelfProfile)
getSelfProfileImpl self = do
  defLocale <- inputs defaultLocale
  mStoredUser <- getUser (tUnqualified self)
  mHackedUser <- traverse hackForBlockingHandleChangeForE2EIdTeams mStoredUser
  let mUser = mkUserFromStored (tDomain self) defLocale <$> mHackedUser
  pure (SelfProfile <$> mUser)
  where
    -- \| This is a hack!
    --
    -- Background:
    -- - https://wearezeta.atlassian.net/browse/WPB-6189.
    -- - comments in `testUpdateHandle` in `/integration`.
    --
    -- FUTUREWORK: figure out a better way for clients to detect E2EId (V6?)
    hackForBlockingHandleChangeForE2EIdTeams :: (Member GalleyAPIAccess r) => StoredUser -> Sem r StoredUser
    hackForBlockingHandleChangeForE2EIdTeams user = do
      e2eid <- hasE2EId user
      pure $
        if e2eid && isJust user.handle
          then user {managedBy = Just ManagedByScim}
          else user

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

-- | Some fields cannot be overwritten by clients for scim-managed users; some others if e2eid
-- is used.  If a client attempts to overwrite any of these, throw `UserSubsystem*ManagedByScim`.
guardLockedFields ::
  ( Member (Error UserSubsystemError) r,
    Member GalleyAPIAccess r
  ) =>
  StoredUser ->
  UpdateOriginType ->
  UserProfileUpdate ->
  Sem r ()
guardLockedFields user updateOrigin (MkUserProfileUpdate {..}) = do
  let idempName = isNothing name || name == Just user.name
      idempLocale = isNothing locale || locale == user.locale
      scim = updateOrigin == UpdateOriginWireClient && user.managedBy == Just ManagedByScim
  e2eid <- hasE2EId user
  when ((scim || e2eid) && not idempName) do
    throw UserSubsystemDisplayNameManagedByScim
  when (scim {- e2eid does not matter, it's not part of the e2eid cert! -} && not idempLocale) do
    throw UserSubsystemLocaleManagedByScim

guardLockedHandleField ::
  ( Member GalleyAPIAccess r,
    Member (Error UserSubsystemError) r
  ) =>
  StoredUser ->
  UpdateOriginType ->
  Handle ->
  Sem r ()
guardLockedHandleField user updateOrigin handle = do
  let idemp = Just handle == user.handle
      scim = updateOrigin == UpdateOriginWireClient && user.managedBy == Just ManagedByScim
      hasHandle = isJust user.handle
  e2eid <- hasE2EId user
  when ((scim || (e2eid && hasHandle)) && not idemp) do
    throw UserSubsystemHandleManagedByScim

updateUserProfileImpl ::
  ( Member UserStore r,
    Member (Error UserSubsystemError) r,
    Member Events r,
    Member GalleyAPIAccess r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  UpdateOriginType ->
  UserProfileUpdate ->
  Sem r ()
updateUserProfileImpl (tUnqualified -> uid) mconn updateOrigin update = do
  user <- getUser uid >>= note UserSubsystemProfileNotFound
  guardLockedFields user updateOrigin update
  mapError (\StoredUserUpdateHandleExists -> UserSubsystemHandleExists) $
    updateUser uid (storedUserUpdate update)
  generateUserEvent uid mconn (mkProfileUpdateEvent uid update)

storedUserUpdate :: UserProfileUpdate -> StoredUserUpdate
storedUserUpdate update =
  MkStoredUserUpdate
    { name = update.name,
      textStatus = update.textStatus,
      pict = update.pict,
      assets = update.assets,
      accentId = update.accentId,
      locale = update.locale,
      supportedProtocols = update.supportedProtocols
    }

mkProfileUpdateEvent :: UserId -> UserProfileUpdate -> UserEvent
mkProfileUpdateEvent uid update =
  UserUpdated $
    (emptyUserUpdatedData uid)
      { eupName = update.name,
        eupTextStatus = update.textStatus,
        eupPict = update.pict,
        eupAccentId = update.accentId,
        eupAssets = update.assets,
        eupLocale = update.locale,
        eupSupportedProtocols = update.supportedProtocols
      }

mkProfileUpdateHandleEvent :: UserId -> Handle -> UserEvent
mkProfileUpdateHandleEvent uid handle =
  UserUpdated $ (emptyUserUpdatedData uid) {eupHandle = Just handle}

getLocalUserAccountByUserKeyImpl ::
  ( Member UserStore r,
    Member UserKeyStore r,
    Member (Input UserSubsystemConfig) r
  ) =>
  Local EmailKey ->
  Sem r (Maybe UserAccount)
getLocalUserAccountByUserKeyImpl target = runMaybeT $ do
  config <- lift input
  uid <- MaybeT $ lookupKey (tUnqualified target)
  user <- MaybeT $ getUser uid
  pure $ mkAccountFromStored (tDomain target) config.defaultLocale user

--------------------------------------------------------------------------------
-- Update Handle

updateHandleImpl ::
  ( Member (Error UserSubsystemError) r,
    Member GalleyAPIAccess r,
    Member Events r,
    Member UserStore r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  UpdateOriginType ->
  Text ->
  Sem r ()
updateHandleImpl (tUnqualified -> uid) mconn updateOrigin uhandle = do
  newHandle :: Handle <- note UserSubsystemInvalidHandle $ Handle.parseHandle uhandle
  when (isBlacklistedHandle newHandle) $
    throw UserSubsystemInvalidHandle
  user <- getUser uid >>= note UserSubsystemNoIdentity
  guardLockedHandleField user updateOrigin newHandle
  when (isNothing user.identity) $
    throw UserSubsystemNoIdentity
  mapError (\StoredUserUpdateHandleExists -> UserSubsystemHandleExists) $
    UserStore.updateUserHandle uid (MkStoredUserHandleUpdate user.handle newHandle)
  generateUserEvent uid mconn (mkProfileUpdateHandleEvent uid newHandle)

checkHandleImpl :: (Member (Error UserSubsystemError) r, Member UserStore r) => Text -> Sem r CheckHandleResp
checkHandleImpl uhandle = do
  xhandle :: Handle <- Handle.parseHandle uhandle & maybe (throw UserSubsystemInvalidHandle) pure
  when (isBlacklistedHandle xhandle) $
    throw UserSubsystemInvalidHandle
  owner <- lookupHandle xhandle
  if isJust owner
    then -- Handle is taken (=> getHandleInfo will return 200)
      pure CheckHandleFound
    else -- Handle is free and can be taken
      pure CheckHandleNotFound

hasE2EId :: (Member GalleyAPIAccess r) => StoredUser -> Sem r Bool
hasE2EId user =
  -- FUTUREWORK(mangoiv): we should use a function 'getSingleFeatureForUser'
  (.status) . npProject @MlsE2EIdConfig
    <$> getAllTeamFeaturesForUser (Just user.id) <&> \case
      FeatureStatusEnabled -> True
      FeatureStatusDisabled -> False

--------------------------------------------------------------------------------
-- Check Handles

-- | checks for handles @check@ to be available and returns
--   at maximum @num@ of them
checkHandlesImpl :: (Member UserStore r) => [Handle] -> Word -> Sem r [Handle]
checkHandlesImpl check num = reverse <$> collectFree [] check num
  where
    collectFree free _ 0 = pure free
    collectFree free [] _ = pure free
    collectFree free (h : hs) n =
      if isBlacklistedHandle h
        then collectFree free hs n
        else do
          owner <- glimpseHandle h
          case owner of
            Nothing -> collectFree (h : free) hs (n - 1)
            Just _ -> collectFree free hs n
