{-# LANGUAGE RecordWildCards #-}

module Wire.UserSubsystem.Interpreter (runUserSubsystem, UserSubsystemConfig (..)) where

import Control.Lens (view)
import Control.Monad.Trans.Maybe
import Data.ByteString.Conversion
import Data.Code
import Data.Either.Extra
import Data.Handle (Handle)
import Data.Handle qualified as Handle
import Data.Id
import Data.Json.Util
import Data.LegalHold
import Data.Misc
import Data.Qualified
import Data.Time.Clock
import Imports hiding (local)
import Polysemy
import Polysemy.Error hiding (try)
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import Servant.Client.Core
import System.Logger.Message
import Wire.API.Federation.API
import Wire.API.Federation.Error
import Wire.API.Password (verifyPassword)
import Wire.API.Team.Feature
import Wire.API.Team.Member hiding (userId)
import Wire.API.User
import Wire.API.UserEvent
import Wire.Arbitrary
import Wire.DeleteQueue
import Wire.EmailSmsSubsystem
import Wire.FederationAPIAccess
import Wire.GalleyAPIAccess
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.PasswordStore (PasswordStore, lookupHashedPassword)
import Wire.Sem.Concurrency
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.StoredUser
import Wire.UserEvents
import Wire.UserKeyStore
import Wire.UserStore as UserStore
import Wire.UserSubsystem
import Wire.UserSubsystem.Error
import Wire.UserSubsystem.HandleBlacklist
import Wire.VerificationCode qualified as VerificationCode
import Wire.VerificationCodeGen
import Wire.VerificationCodeSubsystem

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
    Member UserKeyStore r,
    Member (Concurrency 'Unsafe) r, -- FUTUREWORK: subsystems should implement concurrency inside interpreters, not depend on this dangerous effect.
    Member (Error FederationError) r,
    Member (Error UserSubsystemError) r,
    Member (FederationAPIAccess fedM) r,
    Member DeleteQueue r,
    Member UserEvents r,
    Member Now r,
    RunClient (fedM 'Brig),
    FederationMonad fedM,
    Typeable fedM,
    Member TinyLog r,
    Member VerificationCodeSubsystem r,
    Member EmailSmsSubsystem r,
    Member PasswordStore r
  ) =>
  UserSubsystemConfig ->
  InterpreterFor UserSubsystem r
runUserSubsystem cfg = runInputConst cfg . interpretUserSubsystem . raiseUnder

interpretUserSubsystem ::
  ( Member GalleyAPIAccess r,
    Member UserStore r,
    Member UserKeyStore r,
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
    Typeable fedM,
    Member TinyLog r,
    Member VerificationCodeSubsystem r,
    Member EmailSmsSubsystem r,
    Member PasswordStore r
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
  RequestDeletionCode uid -> requestDeletionCodeImpl uid
  DeleteUserByVerificationCode verification -> deleteUserByVerificationCodeImpl verification
  DeleteUserByPassword luid password -> deleteUserByPasswordImpl luid password

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
  catMaybes <$> traverse (getLocalUserProfileImpl emailVisibilityConfigWithViewer) (sequence luids)
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
    Member UserEvents r,
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
  uid <- MaybeT $ lookupKey (tUnqualified target)
  MaybeT $ getAccountByUserId $ qualifyAs target uid

getAccountByUserId :: (Member UserStore r, Member (Input UserSubsystemConfig) r) => Local UserId -> Sem r (Maybe UserAccount)
getAccountByUserId luid = runMaybeT do
  config <- lift input
  user <- MaybeT $ getUser $ tUnqualified luid
  pure $ mkAccountFromStored (tDomain luid) config.defaultLocale user

--------------------------------------------------------------------------------
-- Update Handle

updateHandleImpl ::
  ( Member (Error UserSubsystemError) r,
    Member GalleyAPIAccess r,
    Member UserEvents r,
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
  wsStatus . afcMlsE2EId
    <$> getAllFeatureConfigsForUser (Just user.id) <&> \case
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

--------------------------------------------------------------------------------
-- Account Deletion

-- | Initiate validation of a user's delete request.  Called via @delete /self@.  Users with an
-- 'UserSSOId' can still do this if they also have an 'Email', 'Phone', and/or password.  Otherwise,
-- the team admin has to delete them via the team console on galley.
--
-- Owners are not allowed to delete themselves.  Instead, they must ask a fellow owner to
-- delete them in the team settings.  This protects teams against orphanhood.
--
-- TODO: communicate deletions of SSO users to SSO service.
requestDeletionCodeImpl ::
  forall r.
  ( Member (Input UserSubsystemConfig) r,
    Member UserStore r,
    Member GalleyAPIAccess r,
    Member (Error UserSubsystemError) r,
    Member VerificationCodeSubsystem r,
    Member TinyLog r,
    Member EmailSmsSubsystem r
  ) =>
  Local UserId ->
  Sem r (Maybe Timeout)
requestDeletionCodeImpl luid =
  mapError UserSubsystemDeleteUserError $ runMaybeT $ do
    account <- MaybeT $ getAccountReadyToBeDeleted luid
    MaybeT case userEmail account.accountUser of
      Just email -> sendCode account email
      Nothing ->
        deleteAccount account >> pure Nothing
  where
    sendCode a target = do
      let gen = mkVerificationCodeGen target
      createCode gen VerificationCode.AccountDeletion (VerificationCode.Retries 3) (VerificationCode.Timeout 600) (Just (toUUID (tUnqualified luid))) >>= \case
        Left (CodeAlreadyExists c) -> throw $ DeleteUserPendingCode (VerificationCode.codeTTL c)
        Right c -> do
          Log.info $
            field "user" (toByteString (tUnqualified luid))
              . msg (val "Sending verification code for account deletion")
          sendAccountDeletionEmail
            target
            a.accountUser.userDisplayName
            c.codeKey
            c.codeValue
            a.accountUser.userLocale
          -- TODO: figure out how to
          -- `onException` lift (liftSem $ deleteCode k VerificationCode.AccountDeletion)
          pure $ Just $ VerificationCode.codeTTL c

deleteUserByVerificationCodeImpl ::
  ( Member VerificationCodeSubsystem r,
    Member (Error UserSubsystemError) r,
    Member (Input UserSubsystemConfig) r,
    Member UserStore r
  ) =>
  Local VerifyDeleteUser ->
  Sem r ()
deleteUserByVerificationCodeImpl luser@(tUnqualified -> usr) = mapError UserSubsystemDeleteUserError do
  verifiedCode <- verifyCode usr.verifyDeleteUserKey VerificationCode.AccountDeletion usr.verifyDeleteUserCode
  uid <- maybe (throw DeleteUserInvalidCode) pure do
    verifiedCode >>= VerificationCode.codeAccount
  account <- getAccountByUserId $ toLocalUnsafe (tDomain luser) (Id uid)
  traverse_ deleteAccount account
  deleteCode usr.verifyDeleteUserKey VerificationCode.AccountDeletion

deleteUserByPasswordImpl ::
  ( Member TinyLog r,
    Member PasswordStore r,
    Member (Error UserSubsystemError) r,
    Member (Input UserSubsystemConfig) r,
    Member GalleyAPIAccess r,
    Member UserStore r
  ) =>
  Local UserId ->
  PlainTextPassword6 ->
  Sem r ()
deleteUserByPasswordImpl luid pw = mapError UserSubsystemDeleteUserError do
  getAccountReadyToBeDeleted luid >>= \case
    Nothing -> pure ()
    Just account -> do
      let uid = tUnqualified luid
      Log.info $
        field "user" (toByteString uid)
          . msg (val "Attempting account deletion with a password")
      actual <- lookupHashedPassword uid
      case actual of
        Nothing -> throw DeleteUserInvalidPassword
        Just p ->
          -- We're deleting a user, no sense in updating their pwd, so we ignore pwd status
          if (verifyPassword pw p)
            then throw DeleteUserInvalidPassword
            else deleteAccount account

deleteAccount :: (Member (Error UserSubsystemError) r) => UserAccount -> Sem r ()
deleteAccount (accountUser -> _user) = mapError UserSubsystemDeleteUserError undefined

--   let uid = user.userId
--   -- we need a domain to qualify, e.g. with Local UserAccount
--   -- luid <- undefined (tDomain luser) uid
--   Log.info $ field "user" (toByteString uid) . msg (val "Deleting account")
--   do
--     -- Free unique keys
--     for_ (userEmail user) $ deleteKeyForUser uid . mkEmailKey
--     clearProperties uid
--     deleteUser user
--
-- this guy is problematic
-- Intra.rmUser uid (userAssets user)

-- lookupClients uid >>= mapM_ (Data.rmClient uid . clientId)

-- Intra.onUserEvent uid Nothing (UserDeleted (tUntagged luid))
-- Note: Connections can only be deleted afterwards, since
--       they need to be notified.
-- Data.deleteConnections uid
-- Auth.revokeAllCookies uid

getAccountReadyToBeDeleted ::
  ( Member (Input UserSubsystemConfig) r,
    Member (Error DeleteUserError) r,
    Member GalleyAPIAccess r,
    Member UserStore r
  ) =>
  Local UserId ->
  Sem r (Maybe UserAccount)
getAccountReadyToBeDeleted luid = do
  account <- getAccountByUserId luid
  case account of
    Nothing -> throw DeleteUserInvalid
    Just a -> case accountStatus a of
      Deleted -> pure Nothing
      Suspended -> ensureNotOwner a $> Just a
      Active -> ensureNotOwner a $> Just a
      Ephemeral -> pure $ Just a
      PendingInvitation -> pure $ Just a
  where
    ensureNotOwner acc = do
      case userTeam $ accountUser acc of
        Nothing -> pure ()
        Just tid -> do
          isOwner <- GalleyAPIAccess.memberIsTeamOwner tid (tUnqualified luid)
          when isOwner $ throw DeleteUserOwnerDeletingSelf
