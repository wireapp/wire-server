{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- TODO: Move to Brig.User.Account
module Brig.API.User
  ( -- * User Accounts / Profiles
    upgradePersonalToTeam,
    createUser,
    createUserSpar,
    createUserInviteViaScim,
    checkRestrictedUserCreation,
    CheckHandleResp (..),
    checkHandle,
    lookupHandle,
    changeAccountStatus,
    changeSingleAccountStatus,
    getLegalHoldStatus,
    Data.lookupName,
    Data.lookupUser,
    Data.lookupRichInfoMultiUsers,
    revokeIdentity,
    deleteUserNoVerify,
    deleteUsersNoVerify,
    deleteSelfUser,
    verifyDeleteUser,
    ensureAccountDeleted,
    deleteAccount,
    checkHandles,
    isBlacklistedHandle,

    -- * Activation
    sendActivationCode,
    preverify,
    activate,
    Brig.API.User.lookupActivationCode,

    -- * Password Management
    changePassword,
    lookupPasswordResetCode,

    -- * Blacklisting
    isBlacklisted,
    blacklistDelete,
    blacklistInsert,

    -- * Utilities
    fetchUserIdentity,
  )
where

import Brig.API.Types
import Brig.API.Util
import Brig.App as App
import Brig.Data.Activation (ActivationEvent (..), activationErrorToRegisterError)
import Brig.Data.Activation qualified as Data
import Brig.Data.Client qualified as Data
import Brig.Data.Connection (countConnections)
import Brig.Data.Connection qualified as Data
import Brig.Data.User
import Brig.Data.User qualified as Data
import Brig.Effects.ConnectionStore
import Brig.Effects.UserPendingActivationStore (UserPendingActivation (..), UserPendingActivationStore)
import Brig.Effects.UserPendingActivationStore qualified as UserPendingActivationStore
import Brig.IO.Intra qualified as Intra
import Brig.Options hiding (internalEvents)
import Brig.Team.Email
import Brig.Types.Activation (ActivationPair)
import Brig.Types.Intra
import Brig.User.Auth.Cookie qualified as Auth
import Cassandra hiding (Set)
import Control.Error
import Control.Lens (preview, to, (^.), _Just)
import Control.Monad.Catch
import Data.ByteString.Conversion
import Data.Code
import Data.Coerce (coerce)
import Data.Currency qualified as Currency
import Data.Handle (Handle (fromHandle))
import Data.Id as Id
import Data.Json.Util
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.List.Extra
import Data.List1 as List1 (List1, singleton)
import Data.Misc
import Data.Qualified
import Data.Range
import Data.Time.Clock
import Data.UUID.V4 (nextRandom)
import Imports
import Network.Wai.Utilities
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import Prometheus qualified as Prom
import System.Logger.Message
import UnliftIO.Async (mapConcurrently_)
import Wire.API.Connection
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Routes.Internal.Galley.TeamsIntra qualified as Team
import Wire.API.Team hiding (newTeam)
import Wire.API.Team.Member (legalHoldStatus)
import Wire.API.Team.Role
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Client
import Wire.API.User.RichInfo
import Wire.API.UserEvent
import Wire.ActivationCodeStore
import Wire.ActivationCodeStore qualified as ActivationCode
import Wire.AuthenticationSubsystem (AuthenticationSubsystem, internalLookupPasswordResetCode)
import Wire.BlockListStore as BlockListStore
import Wire.DeleteQueue
import Wire.EmailSending
import Wire.EmailSubsystem
import Wire.Error
import Wire.Events (Events)
import Wire.Events qualified as Events
import Wire.GalleyAPIAccess as GalleyAPIAccess
import Wire.HashPassword (HashPassword)
import Wire.HashPassword qualified as HashPassword
import Wire.InvitationStore (InvitationStore, StoredInvitation)
import Wire.InvitationStore qualified as InvitationStore
import Wire.NotificationSubsystem
import Wire.PasswordResetCodeStore (PasswordResetCodeStore)
import Wire.PasswordStore (PasswordStore, lookupHashedPassword, upsertHashedPassword)
import Wire.PropertySubsystem as PropertySubsystem
import Wire.Sem.Concurrency
import Wire.Sem.Paging.Cassandra
import Wire.UserKeyStore
import Wire.UserStore
import Wire.UserSubsystem as User
import Wire.UserSubsystem.HandleBlacklist
import Wire.VerificationCode qualified as VerificationCode
import Wire.VerificationCodeGen (mkVerificationCodeGen)
import Wire.VerificationCodeSubsystem

-------------------------------------------------------------------------------
-- Create User

data IdentityError
  = IdentityErrorBlacklistedEmail
  | IdentityErrorUserKeyExists

identityErrorToRegisterError :: IdentityError -> RegisterError
identityErrorToRegisterError = \case
  IdentityErrorBlacklistedEmail -> RegisterErrorBlacklistedEmail
  IdentityErrorUserKeyExists -> RegisterErrorUserKeyExists

identityErrorToBrigError :: IdentityError -> HttpError
identityErrorToBrigError = \case
  IdentityErrorBlacklistedEmail -> StdError $ errorToWai @'E.BlacklistedEmail
  IdentityErrorUserKeyExists -> StdError $ errorToWai @'E.UserKeyExists

verifyUniquenessAndCheckBlacklist ::
  ( Member BlockListStore r,
    Member UserKeyStore r
  ) =>
  EmailKey ->
  ExceptT IdentityError (AppT r) ()
verifyUniquenessAndCheckBlacklist uk = do
  checkKey Nothing uk
  blacklisted <- lift $ liftSem $ BlockListStore.exists uk
  when blacklisted $ throwE IdentityErrorBlacklistedEmail
  where
    checkKey u k = do
      av <- lift $ liftSem $ keyAvailable k u
      unless av $
        throwE IdentityErrorUserKeyExists

createUserSpar ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member TinyLog r,
    Member UserSubsystem r,
    Member HashPassword r,
    Member Events r
  ) =>
  NewUserSpar ->
  ExceptT CreateUserSparError (AppT r) CreateUserResult
createUserSpar new = do
  let handle' = newUserSparHandle new
      new' = newUserFromSpar new
      ident = newUserSparSSOId new
      tid = newUserSparTeamId new

  -- Create account
  account <- lift $ do
    (account, pw) <- newAccount new' Nothing (Just tid) handle'

    let uid = userId account

    -- FUTUREWORK: make this transactional if possible
    wrapClient $ Data.insertAccount account Nothing pw False
    case unRichInfo <$> newUserSparRichInfo new of
      Just richInfo -> wrapClient $ Data.updateRichInfo uid richInfo
      Nothing -> pure () -- Nothing to do
    liftSem $ GalleyAPIAccess.createSelfConv uid
    liftSem $ User.internalUpdateSearchIndex uid
    liftSem $ Events.generateUserEvent uid Nothing (UserCreated account)

    pure account

  -- Add to team
  userTeam <- withExceptT CreateUserSparRegistrationError $ addUserToTeamSSO account tid (SSOIdentity ident Nothing) (newUserSparRole new)

  -- Set up feature flags
  luid <- lift $ ensureLocal (userQualifiedId account)
  lift $ initAccountFeatureConfig (tUnqualified luid)

  -- Set handle
  lift $ updateHandle' luid handle'

  pure $! CreateUserResult account Nothing (Just userTeam)
  where
    updateHandle' :: Local UserId -> Maybe Handle -> AppT r ()
    updateHandle' _ Nothing = pure ()
    updateHandle' luid (Just h) =
      liftSem $ User.updateHandle luid Nothing UpdateOriginScim (fromHandle h)

    addUserToTeamSSO :: User -> TeamId -> UserIdentity -> Role -> ExceptT RegisterError (AppT r) CreateUserTeam
    addUserToTeamSSO account tid ident role = do
      let uid = userId account
      added <- lift $ liftSem $ GalleyAPIAccess.addTeamMember uid tid Nothing role
      unless added $
        throwE RegisterErrorTooManyTeamMembers
      lift $ do
        wrapClient $ activateUser uid ident
        void $ onActivated (AccountActivated account)
        liftSem $
          Log.info $
            field "user" (toByteString uid)
              . field "team" (toByteString tid)
              . msg (val "Added via SSO")
      Team.TeamName nm <- lift $ liftSem $ GalleyAPIAccess.getTeamName tid
      pure $ CreateUserTeam tid nm

upgradePersonalToTeam ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member UserStore r,
    Member UserSubsystem r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r,
    Member EmailSending r
  ) =>
  Local UserId ->
  BindingNewTeamUser ->
  ExceptT UpgradePersonalToTeamError (AppT r) CreateUserTeam
upgradePersonalToTeam luid bNewTeam = do
  -- check that the user is not part of a team
  mSelfProfile <- lift $ liftSem $ getSelfProfile luid
  user <-
    maybe
      (throwE UpgradePersonalToTeamErrorUserNotFound)
      (pure . selfUser)
      mSelfProfile
  when (isJust user.userTeam) $
    throwE UpgradePersonalToTeamErrorAlreadyInATeam

  lift $ do
    liftSem $
      for_ (userEmail user) guardUpgradePersonalUserToTeamEmailDomain
    -- generate team ID
    tid <- randomId

    let uid = tUnqualified luid
    createUserTeam <- do
      liftSem $ GalleyAPIAccess.createTeam uid (bnuTeam bNewTeam) tid
      let newTeam = bNewTeam.bnuTeam
      pure $ CreateUserTeam tid (fromRange newTeam.newTeamName)
    liftSem $ GalleyAPIAccess.changeTeamStatus tid Team.Active bNewTeam.bnuCurrency

    liftSem $ updateUserTeam uid tid
    liftSem $ User.internalUpdateSearchIndex uid
    liftSem $ Intra.sendUserEvent uid Nothing (teamUpdated uid tid)
    initAccountFeatureConfig uid

    -- send confirmation email
    for_ (userEmail user) $ \email -> do
      sendNewTeamOwnerWelcomeEmail
        email
        tid
        bNewTeam.bnuTeam.newTeamName.fromRange
        (Just user.userLocale)
        user.userDisplayName

    pure $! createUserTeam

-- docs/reference/user/registration.md {#RefRegistration}
createUser ::
  forall r p.
  ( Member BlockListStore r,
    Member GalleyAPIAccess r,
    Member (UserPendingActivationStore p) r,
    Member UserKeyStore r,
    Member UserSubsystem r,
    Member TinyLog r,
    Member Events r,
    Member (Input (Local ())) r,
    Member PasswordResetCodeStore r,
    Member HashPassword r,
    Member InvitationStore r,
    Member ActivationCodeStore r
  ) =>
  NewUser ->
  ExceptT RegisterError (AppT r) CreateUserResult
createUser new = do
  email <- fetchAndValidateEmail new

  -- get invitation and existing account
  (mNewTeamUser, teamInvitation, tid) <-
    case newUserTeam new of
      Just (NewTeamMember i) -> do
        inv <- lift $ liftSem $ internalFindTeamInvitation (mkEmailKey <$> email) i
        pure (Nothing, Just inv, Just inv.teamId)
      Just (NewTeamCreator t) -> do
        for_ (emailIdentity =<< new.newUserIdentity) (lift . liftSem . guardRegisterActivateUserEmailDomain)
        (Just t,Nothing,) <$> (Just . Id <$> liftIO nextRandom)
      Just (NewTeamMemberSSO tid) ->
        pure (Nothing, Nothing, Just tid)
      Nothing -> do
        for_ (emailIdentity =<< new.newUserIdentity) (lift . liftSem . guardRegisterActivateUserEmailDomain)
        pure (Nothing, Nothing, Nothing)
  let mbInv = (.invitationId) <$> teamInvitation
  mbExistingAccount <-
    lift $
      join
        <$> for mbInv do
          \invid -> liftSem $ do
            luid :: Local UserId <- qualifyLocal' (coerce invid)
            User.getLocalAccountBy WithPendingInvitations luid

  let (new', mbHandle) = case mbExistingAccount of
        Nothing ->
          ( new {newUserIdentity = newIdentity email (newUserSSOId new)},
            Nothing
          )
        Just existingAccount ->
          let mbSSOid =
                case (teamInvitation, email, existingAccount.userManagedBy, userSSOId existingAccount) of
                  -- isJust teamInvitation And ManagedByScim implies that the
                  -- user invitation has been generated by SCIM and there is no IdP
                  (Just _, _, ManagedByScim, ssoId@(Just (UserScimExternalId _))) ->
                    -- if the existing user has an external ID, we have to use it because it can differ from the email address
                    ssoId
                  (Just _, Just em, ManagedByScim, _) ->
                    Just $ UserScimExternalId (fromEmail em)
                  _ -> newUserSSOId new
           in ( new
                  { newUserManagedBy = Just existingAccount.userManagedBy,
                    newUserIdentity = newIdentity email mbSSOid
                  },
                existingAccount.userHandle
              )

  -- Create account
  account <- lift $ do
    (account, pw) <- newAccount new' mbInv tid mbHandle

    let uid = userId account
    liftSem $ do
      Log.debug $ field "user" (toByteString uid) . field "action" (val "User.createUser")
      Log.info $ field "user" (toByteString uid) . msg (val "Creating user")

    wrapClient $ Data.insertAccount account Nothing pw False
    liftSem $ GalleyAPIAccess.createSelfConv uid
    liftSem $ Events.generateUserEvent uid Nothing (UserCreated account)

    pure account

  let uid = qUnqualified account.userQualifiedId

  createUserTeam <- do
    activatedTeam <- lift $ do
      case (tid, mNewTeamUser) of
        (Just tid', Just newTeamUser) -> do
          liftSem $ GalleyAPIAccess.createTeam uid (bnuTeam newTeamUser) tid'
          let activating = isJust (newUserEmailCode new)
              newTeam = newTeamUser.bnuTeam
          pure $
            if activating
              then Just $ CreateUserTeam tid' (fromRange newTeam.newTeamName)
              else Nothing
        _ -> pure Nothing

    joinedTeamInvite <- case teamInvitation of
      Just inv -> do
        acceptInvitationToTeam account inv (mkEmailKey inv.email) (EmailIdentity inv.email)
        Team.TeamName nm <- lift $ liftSem $ GalleyAPIAccess.getTeamName inv.teamId
        pure (Just $ CreateUserTeam inv.teamId nm)
      Nothing -> pure Nothing

    joinedTeamSSO <- case (newUserIdentity new', tid) of
      (Just ident@(SSOIdentity (UserSSOId _) _), Just tid') -> Just <$> addUserToTeamSSO account tid' ident
      _ -> pure Nothing

    pure (activatedTeam <|> joinedTeamInvite <|> joinedTeamSSO)

  edata <-
    if isJust teamInvitation
      then pure Nothing
      else handleEmailActivation email uid mNewTeamUser

  lift $ initAccountFeatureConfig uid

  pure $! CreateUserResult account edata createUserTeam
  where
    -- NOTE: all functions in the where block don't use any arguments of createUser

    fetchAndValidateEmail :: NewUser -> ExceptT RegisterError (AppT r) (Maybe EmailAddress)
    fetchAndValidateEmail newUser = do
      let email = newUserEmail newUser
      for_ (mkEmailKey <$> email) $ \k ->
        verifyUniquenessAndCheckBlacklist k !>> identityErrorToRegisterError
      pure email

    acceptInvitationToTeam ::
      User ->
      StoredInvitation ->
      EmailKey ->
      UserIdentity ->
      ExceptT RegisterError (AppT r) ()
    acceptInvitationToTeam account inv uk ident = do
      let uid = userId account
      ok <- lift $ liftSem $ claimKey uk uid
      unless ok $
        throwE RegisterErrorUserKeyExists
      let minvmeta :: Maybe (UserId, UTCTimeMillis)
          minvmeta = (,inv.createdAt) <$> inv.createdBy
          role :: Role
          role = fromMaybe defaultRole inv.role
      added <- lift $ liftSem $ GalleyAPIAccess.addTeamMember uid inv.teamId minvmeta role
      unless added $
        throwE RegisterErrorTooManyTeamMembers
      lift $ do
        -- ('insertAccount' sets column activated to False; here it is set to True.)
        wrapClient $ activateUser uid ident
        void $ onActivated (AccountActivated account)
        liftSem do
          Log.info $
            field "user" (toByteString uid)
              . field "team" (toByteString $ inv.teamId)
              . msg (val "Accepting invitation")
          UserPendingActivationStore.remove uid
          InvitationStore.deleteInvitation inv.teamId inv.invitationId

    addUserToTeamSSO :: User -> TeamId -> UserIdentity -> ExceptT RegisterError (AppT r) CreateUserTeam
    addUserToTeamSSO account tid ident = do
      let uid = userId account
      added <- lift $ liftSem $ GalleyAPIAccess.addTeamMember uid tid Nothing defaultRole
      unless added $
        throwE RegisterErrorTooManyTeamMembers
      lift $ do
        wrapClient $ activateUser uid ident
        void $ onActivated (AccountActivated account)
        liftSem $
          Log.info $
            field "user" (toByteString uid)
              . field "team" (toByteString tid)
              . msg (val "Added via SSO")
      Team.TeamName nm <- lift $ liftSem $ GalleyAPIAccess.getTeamName tid
      pure $ CreateUserTeam tid nm

    -- Handle e-mail activation (deprecated, see #RefRegistrationNoPreverification in /docs/reference/user/registration.md)
    handleEmailActivation ::
      Maybe EmailAddress ->
      UserId ->
      Maybe BindingNewTeamUser ->
      ExceptT RegisterError (AppT r) (Maybe Activation)
    handleEmailActivation email uid newTeam = do
      fmap join . for (mkEmailKey <$> email) $ \ek -> case newUserEmailCode new of
        Nothing -> do
          timeout <- asks (.settings.activationTimeout)
          lift . liftSem $ do
            edata <- newActivationCode ek timeout (Just uid)
            Log.info $
              field "user" (toByteString uid)
                . field "activation.key" (toByteString $ activationKey edata)
                . msg (val "Created email activation key/code pair")
            pure $ Just edata
        Just c -> do
          ak <- liftIO $ Data.mkActivationKey ek
          void $
            activateWithCurrency (ActivateKey ak) c (Just uid) (bnuCurrency =<< newTeam)
              !>> activationErrorToRegisterError
          pure Nothing

initAccountFeatureConfig :: UserId -> (AppT r) ()
initAccountFeatureConfig uid = do
  mStatus <- preview (App.settingsLens . featureFlagsLens . _Just . to conferenceCalling . to forNew . _Just)
  wrapClient $ traverse_ (Data.updateFeatureConferenceCalling uid . Just) mStatus

-- | 'createUser' is becoming hard to maintain, and instead of adding more case distinctions
-- all over the place there, we add a new function that handles just the one new flow where
-- users are invited to the team via scim.
createUserInviteViaScim ::
  ( Member BlockListStore r,
    Member UserKeyStore r,
    Member (UserPendingActivationStore p) r,
    Member TinyLog r
  ) =>
  NewUserScimInvitation ->
  ExceptT HttpError (AppT r) User
createUserInviteViaScim (NewUserScimInvitation tid uid extId loc name email _) = do
  let emKey = mkEmailKey email
  verifyUniquenessAndCheckBlacklist emKey !>> identityErrorToBrigError
  account <- lift . wrapClient $ newAccountInviteViaScim uid extId tid loc name email
  lift . liftSem . Log.debug $ field "user" (toByteString . userId $ account) . field "action" (val "User.createUserInviteViaScim")

  -- add the expiry table entry first!  (if brig creates an account, and then crashes before
  -- creating the expiry table entry, gc will miss user data.)
  expiresAt <- do
    ttl <- asks (.settings.teamInvitationTimeout)
    now <- liftIO =<< asks (.currentTime)
    pure $ addUTCTime (realToFrac ttl) now
  lift . liftSem $ UserPendingActivationStore.add (UserPendingActivation uid expiresAt)

  let activated =
        -- treating 'PendingActivation' as 'Active', but then 'Brig.Data.User.toIdentity'
        -- would not produce an identity, and so we won't have the email address to construct
        -- the SCIM user.
        True
  lift . wrapClient $ Data.insertAccount account Nothing Nothing activated
  pure account

-- | docs/reference/user/registration.md {#RefRestrictRegistration}.
checkRestrictedUserCreation :: NewUser -> ExceptT RegisterError (AppT r) ()
checkRestrictedUserCreation new = do
  restrictPlease <- fromMaybe False <$> asks (.settings.restrictUserCreation)
  when
    ( restrictPlease
        && not (isNewUserTeamMember new)
        && not (isNewUserEphemeral new)
    )
    $ throwE RegisterErrorUserCreationRestricted

-------------------------------------------------------------------------------
-- Forcefully revoke a verified identity

-- | Now that a user can only have an email-based identity, revoking an identity
-- boils down to deactivating the user.
revokeIdentity ::
  ( Member UserSubsystem r,
    Member UserKeyStore r
  ) =>
  EmailAddress ->
  AppT r ()
revokeIdentity key = do
  mu <- liftSem . lookupKey . mkEmailKey $ key
  for_ mu $ \u -> do
    deactivate <- maybe False (not . isSSOIdentity) <$> fetchUserIdentity u
    when deactivate . wrapClient . Data.deactivateUser $ u

-------------------------------------------------------------------------------
-- Change Account Status

changeAccountStatus ::
  forall r.
  ( Member (Embed HttpClientIO) r,
    Member (Concurrency 'Unsafe) r,
    Member UserSubsystem r,
    Member Events r
  ) =>
  List1 UserId ->
  AccountStatus ->
  ExceptT AccountStatusError (AppT r) ()
changeAccountStatus usrs status = do
  ev <- mkUserEvent usrs status
  lift $ liftSem $ unsafePooledMapConcurrentlyN_ 16 (update ev) usrs
  where
    update ::
      (UserId -> UserEvent) ->
      UserId ->
      Sem r ()
    update ev u = do
      embed $ Data.updateStatus u status
      User.internalUpdateSearchIndex u
      Events.generateUserEvent u Nothing (ev u)

changeSingleAccountStatus ::
  ( Member UserSubsystem r,
    Member Events r
  ) =>
  UserId ->
  AccountStatus ->
  ExceptT AccountStatusError (AppT r) ()
changeSingleAccountStatus uid status = do
  unlessM (wrapClientE $ Data.userExists uid) $ throwE AccountNotFound
  ev <- mkUserEvent (List1.singleton uid) status
  lift $ do
    wrapClient $ Data.updateStatus uid status
    liftSem $ User.internalUpdateSearchIndex uid
    liftSem $ Events.generateUserEvent uid Nothing (ev uid)

mkUserEvent :: (Traversable t) => t UserId -> AccountStatus -> ExceptT AccountStatusError (AppT r) (UserId -> UserEvent)
mkUserEvent usrs status =
  case status of
    Active -> pure UserResumed
    Suspended -> do
      lift $ wrapHttpClient (mapConcurrently_ Auth.revokeAllCookies usrs)
      pure UserSuspended
    Deleted -> throwE InvalidAccountStatus
    Ephemeral -> throwE InvalidAccountStatus
    PendingInvitation -> throwE InvalidAccountStatus

-------------------------------------------------------------------------------
-- Activation

activate ::
  ( Member GalleyAPIAccess r,
    Member TinyLog r,
    Member Events r,
    Member PasswordResetCodeStore r,
    Member UserSubsystem r
  ) =>
  ActivationTarget ->
  ActivationCode ->
  -- | The user for whom to activate the key.
  Maybe UserId ->
  ExceptT ActivationError (AppT r) ActivationResult
activate tgt code usr = activateWithCurrency tgt code usr Nothing

activateWithCurrency ::
  ( Member GalleyAPIAccess r,
    Member TinyLog r,
    Member Events r,
    Member PasswordResetCodeStore r,
    Member UserSubsystem r
  ) =>
  ActivationTarget ->
  ActivationCode ->
  -- | The user for whom to activate the key.
  Maybe UserId ->
  -- | Potential currency update.
  Maybe Currency.Alpha ->
  ExceptT ActivationError (AppT r) ActivationResult
activateWithCurrency tgt code usr cur = do
  key <- wrapClientE $ mkActivationKey tgt
  lift . liftSem . Log.info $
    field "activation.key" (toByteString key)
      . field "activation.code" (toByteString code)
      . msg (val "Activating")
  event <- Data.activateKey key code usr
  case event of
    Nothing -> pure ActivationPass
    Just e -> do
      (uid, ident, first) <- lift $ onActivated e
      when first $
        lift $
          activateTeam uid
      pure $ ActivationSuccess ident first
  where
    activateTeam uid = do
      tid <- liftSem $ GalleyAPIAccess.getTeamId uid
      for_ tid $ \t -> liftSem $ GalleyAPIAccess.changeTeamStatus t Team.Active cur

preverify ::
  ( MonadClient m,
    MonadReader Env m
  ) =>
  ActivationTarget ->
  ActivationCode ->
  ExceptT ActivationError m (EmailKey, Maybe UserId)
preverify tgt code = do
  key <- mkActivationKey tgt
  Data.verifyCode key code

onActivated ::
  ( Member TinyLog r,
    Member UserSubsystem r,
    Member Events r
  ) =>
  ActivationEvent ->
  AppT r (UserId, Maybe UserIdentity, Bool)
onActivated (AccountActivated account) = liftSem $ do
  let uid = userId account
  Log.debug $ field "user" (toByteString uid) . field "action" (val "User.onActivated")
  Log.info $ field "user" (toByteString uid) . msg (val "User activated")
  User.internalUpdateSearchIndex uid
  Events.generateUserEvent uid Nothing $ UserActivated account
  pure (uid, userIdentity account, True)
onActivated (EmailActivated uid email) = do
  liftSem $ User.internalUpdateSearchIndex uid
  liftSem $ Events.generateUserEvent uid Nothing (emailUpdated uid email)
  wrapHttpClient $ Data.deleteEmailUnvalidated uid
  pure (uid, Just (EmailIdentity email), False)

-- docs/reference/user/activation.md {#RefActivationRequest}
sendActivationCode ::
  forall r.
  ( Member BlockListStore r,
    Member EmailSubsystem r,
    Member GalleyAPIAccess r,
    Member ActivationCodeStore r,
    Member UserKeyStore r
  ) =>
  EmailAddress ->
  Maybe Locale ->
  ExceptT SendActivationCodeError (AppT r) ()
sendActivationCode email loc = do
  let ek = mkEmailKey email
  doesExist <- lift $ liftSem $ isJust <$> lookupKey ek
  when doesExist $
    throwE $
      UserKeyInUse ek
  blacklisted <- lift . liftSem $ BlockListStore.exists ek
  when blacklisted $
    throwE (ActivationBlacklistedUserKey ek)
  uc <- lift . liftSem $ ActivationCode.lookupActivationCode ek
  case uc of
    Nothing -> sendVerificationEmail ek Nothing -- Fresh code request, no user
    Just (Nothing, c) -> sendVerificationEmail ek (Just c) -- Re-requesting existing code
    Just (Just uid, c) -> sendActivationEmail ek c uid -- User re-requesting activation
  where
    notFound = throwM . UserDisplayNameNotFound
    mkPair ::
      EmailKey ->
      Maybe ActivationCode ->
      Maybe UserId ->
      ExceptT SendActivationCodeError (AppT r) (ActivationKey, ActivationCode)
    mkPair k c u = do
      timeout <- asks (.settings.activationTimeout)
      case c of
        Just c' -> liftIO $ (,c') <$> Data.mkActivationKey k
        Nothing -> lift . liftSem $ do
          dat <- newActivationCode k timeout u
          pure (activationKey dat, activationCode dat)
    sendVerificationEmail ek uc = do
      (key, code) <- mkPair ek uc Nothing
      let em = emailKeyOrig ek
      lift $ liftSem $ sendVerificationMail em key code loc
    sendActivationEmail ek uc uid = do
      -- FUTUREWORK(fisx): we allow for 'PendingInvitations' here, but I'm not sure this
      -- top-level function isn't another piece of a deprecated onboarding flow?
      u <- maybe (notFound uid) pure =<< lift (wrapClient $ Data.lookupUser WithPendingInvitations uid)
      (aKey, aCode) <- mkPair ek (Just uc) (Just uid)
      let ident = userIdentity u
          name = userDisplayName u
          loc' = loc <|> Just (userLocale u)
          em = emailKeyOrig ek
      lift $ do
        -- Get user's team, if any.
        mbTeam <- mapM (fmap Team.tdTeam . liftSem . GalleyAPIAccess.getTeam) (userTeam u)
        -- Depending on whether the user is a team creator, send either
        -- a team activation email or a regular email. Note that we
        -- don't have to check if the team is binding because if the
        -- user has 'userTeam' set, it must be binding.
        case mbTeam of
          Just team
            | team ^. teamCreator == uid ->
                liftSem $ sendTeamActivationMail em name aKey aCode loc' (team ^. teamName)
          _otherwise ->
            liftSem $ (maybe sendActivationMail (const sendEmailAddressUpdateMail) ident) em name aKey aCode loc'

mkActivationKey :: (MonadClient m, MonadReader Env m) => ActivationTarget -> ExceptT ActivationError m ActivationKey
mkActivationKey (ActivateKey k) = pure k
mkActivationKey (ActivateEmail e) =
  liftIO $ Data.mkActivationKey (mkEmailKey e)

-------------------------------------------------------------------------------
-- Password Management

changePassword ::
  ( Member PasswordStore r,
    Member UserStore r,
    Member HashPassword r
  ) =>
  UserId ->
  PasswordChange ->
  ExceptT ChangePasswordError (AppT r) ()
changePassword uid cp = do
  activated <- lift $ liftSem $ isActivated uid
  unless activated $
    throwE ChangePasswordNoIdentity
  currpw <- lift $ liftSem $ lookupHashedPassword uid
  let newpw = cp.newPassword
  hashedNewPw <- lift . liftSem $ HashPassword.hashPassword8 newpw
  case (currpw, cp.oldPassword) of
    (Nothing, _) -> lift . liftSem $ upsertHashedPassword uid hashedNewPw
    (Just _, Nothing) -> throwE InvalidCurrentPassword
    (Just pw, Just pw') -> do
      -- We are updating the pwd here anyway, so we don't care about the pwd status
      unlessM (lift . liftSem $ HashPassword.verifyPassword pw' pw) $
        throwE InvalidCurrentPassword
      whenM (lift . liftSem $ HashPassword.verifyPassword newpw pw) $
        throwE ChangePasswordMustDiffer
      lift $ liftSem (upsertHashedPassword uid hashedNewPw) >> wrapClient (Auth.revokeAllCookies uid)

-------------------------------------------------------------------------------
-- User Deletion

-- | Initiate validation of a user's delete request.  Called via @delete /self@.  Users with an
-- 'UserSSOId' can still do this if they also have an 'Email' and/or password.  Otherwise,
-- the team admin has to delete them via the team console on galley.
--
-- Owners are not allowed to delete themselves.  Instead, they must ask a fellow owner to
-- delete them in the team settings.  This protects teams against orphanhood.
--
-- TODO: communicate deletions of SSO users to SSO service.
--
-- FUTUREWORK(mangoiv): this uses 'UserStore', hence it must be moved to 'UserSubsystem'
-- as an effet operation
deleteSelfUser ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member UserKeyStore r,
    Member NotificationSubsystem r,
    Member PasswordStore r,
    Member UserStore r,
    Member EmailSubsystem r,
    Member VerificationCodeSubsystem r,
    Member Events r,
    Member UserSubsystem r,
    Member PropertySubsystem r,
    Member HashPassword r
  ) =>
  Local UserId ->
  Maybe PlainTextPassword6 ->
  ExceptT DeleteUserError (AppT r) (Maybe Timeout)
deleteSelfUser luid@(tUnqualified -> uid) pwd = do
  account <- lift . liftSem $ User.getAccountNoFilter luid
  case account of
    Nothing -> throwE DeleteUserInvalid
    Just a -> case userStatus a of
      Deleted -> pure Nothing
      Suspended -> ensureNotOwner a >> go a
      Active -> ensureNotOwner a >> go a
      Ephemeral -> go a
      PendingInvitation -> go a
  where
    ensureNotOwner :: User -> ExceptT DeleteUserError (AppT r) ()
    ensureNotOwner acc = do
      case userTeam acc of
        Nothing -> pure ()
        Just tid -> do
          isOwner <- lift $ liftSem $ GalleyAPIAccess.memberIsTeamOwner tid uid
          when isOwner $ throwE DeleteUserOwnerDeletingSelf
    go a = maybe (byIdentity a) (byPassword a) pwd
    byIdentity a = case emailIdentity =<< userIdentity a of
      Just email -> sendCode a email
      Nothing -> case pwd of
        Just _ -> throwE DeleteUserMissingPassword
        Nothing -> lift . liftSem $ deleteAccount a >> pure Nothing
    byPassword a pw = do
      lift . liftSem . Log.info $
        field "user" (toByteString uid)
          . msg (val "Attempting account deletion with a password")
      actual <- lift $ liftSem $ lookupHashedPassword uid
      case actual of
        Nothing -> throwE DeleteUserInvalidPassword
        Just p -> do
          -- We're deleting a user, no sense in updating their pwd, so we ignore pwd status
          unlessM (lift . liftSem $ HashPassword.verifyPassword pw p) $
            throwE DeleteUserInvalidPassword
          lift . liftSem $ deleteAccount a >> pure Nothing
    sendCode a target = do
      let gen = mkVerificationCodeGen target
      (lift . liftSem $ createCode gen VerificationCode.AccountDeletion (VerificationCode.Retries 3) (VerificationCode.Timeout 600) (Just (toUUID uid))) >>= \case
        Left (CodeAlreadyExists c) -> throwE $! DeleteUserPendingCode (VerificationCode.codeTTL c)
        Right c -> do
          lift . liftSem . Log.info $
            field "user" (toByteString uid)
              . msg (val "Sending verification code for account deletion")
          let k = VerificationCode.codeKey c
          let v = VerificationCode.codeValue c
          let l = userLocale a
          let n = userDisplayName a
          lift (liftSem $ sendAccountDeletionEmail target n k v l)
            `onException` lift (liftSem $ deleteCode k VerificationCode.AccountDeletion)
          pure $! Just $! VerificationCode.codeTTL c

-- | Conclude validation and scheduling of user's deletion request that was initiated in
-- 'deleteUser'.  Called via @post /delete@.
--
-- FUTUREWORK(mangoiv): this uses 'UserStore', hence it must be moved to 'UserSubsystem'
-- as an effet operation
verifyDeleteUser ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member UserKeyStore r,
    Member TinyLog r,
    Member UserStore r,
    Member VerificationCodeSubsystem r,
    Member Events r,
    Member UserSubsystem r,
    Member PropertySubsystem r
  ) =>
  VerifyDeleteUser ->
  ExceptT DeleteUserError (AppT r) ()
verifyDeleteUser d = do
  let key = verifyDeleteUserKey d
  let code = verifyDeleteUserCode d
  c <- lift . liftSem $ verifyCode key VerificationCode.AccountDeletion code
  a <- maybe (throwE DeleteUserInvalidCode) pure (VerificationCode.codeAccount =<< c)
  luid <- qualifyLocal $ Id a
  account <- lift . liftSem $ User.getAccountNoFilter luid
  for_ account $ lift . liftSem . deleteAccount
  lift . liftSem $ deleteCode key VerificationCode.AccountDeletion

-- | Check if `deleteAccount` succeeded and run it again if needed.
-- Called via @delete /i/user/:uid@.
--
-- FUTUREWORK(mangoiv): this uses 'UserStore', hence it must be moved to 'UserSubsystem'
-- as an effet operation
ensureAccountDeleted ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member UserKeyStore r,
    Member UserStore r,
    Member Events r,
    Member UserSubsystem r,
    Member PropertySubsystem r
  ) =>
  Local UserId ->
  AppT r DeleteUserResult
ensureAccountDeleted luid@(tUnqualified -> uid) = do
  mbAcc <- liftSem $ User.getAccountNoFilter luid
  case mbAcc of
    Nothing -> pure NoUser
    Just acc -> do
      probs <- liftSem $ getPropertyKeys uid

      clients <- wrapClient $ Data.lookupClients uid

      localUid <- qualifyLocal uid
      conCount <- wrapClient $ countConnections localUid [(minBound @Relation) .. maxBound]
      cookies <- wrapClient $ Auth.listCookies uid []

      if notNull probs
        || not (userDeleted acc)
        || notNull clients
        || conCount > 0
        || notNull cookies
        then do
          liftSem $ deleteAccount acc
          pure AccountDeleted
        else pure AccountAlreadyDeleted

-- | Internal deletion without validation.
--
-- Called via @delete /i/user/:uid@ (through `ensureAccountDeleted`), or
-- indirectly via deleting self. Team owners can be deleted if the team is not
-- orphaned, i.e. there is at least one other owner left.
--
-- N.B.: As Cassandra doesn't support transactions, the order of database
-- statements matters! Other functions reason upon some states to imply other
-- states. Please change this order only with care!
--
-- FUTUREWORK(mangoiv): this uses 'UserStore', hence it must be moved to 'UserSubsystem'
-- as an effet operation
-- FUTUREWORK: this does not need the whole User structure, only the User.
deleteAccount ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member UserKeyStore r,
    Member TinyLog r,
    Member UserStore r,
    Member PropertySubsystem r,
    Member UserSubsystem r,
    Member Events r
  ) =>
  User ->
  Sem r ()
deleteAccount user = do
  let uid = userId user
  Log.info $ field "user" (toByteString uid) . msg (val "Deleting account")
  do
    -- Free unique keys
    for_ (userEmail user) $ deleteKeyForUser uid . mkEmailKey

    PropertySubsystem.onUserDeleted uid

    deleteUser user

  Intra.rmUser uid (userAssets user)
  embed $ Data.lookupClients uid >>= mapM_ (Data.rmClient uid . clientId)
  luid <- embed $ qualifyLocal uid
  User.internalUpdateSearchIndex uid
  Events.generateUserEvent uid Nothing (UserDeleted (tUntagged luid))
  embed do
    -- Note: Connections can only be deleted afterwards, since
    --       they need to be notified.
    Data.deleteConnections uid
    Auth.revokeAllCookies uid

-------------------------------------------------------------------------------
-- Lookups

lookupActivationCode ::
  ( Member ActivationCodeStore r,
    Member (Embed IO) r
  ) =>
  EmailAddress ->
  Sem r (Maybe ActivationPair)
lookupActivationCode email = do
  let uk = mkEmailKey email
  k <- liftIO $ Data.mkActivationKey uk
  c <- fmap snd <$> ActivationCode.lookupActivationCode uk
  pure $ (k,) <$> c

lookupPasswordResetCode ::
  ( Member AuthenticationSubsystem r
  ) =>
  EmailAddress ->
  (AppT r) (Maybe PasswordResetPair)
lookupPasswordResetCode =
  liftSem . internalLookupPasswordResetCode . mkEmailKey

deleteUserNoVerify ::
  (Member DeleteQueue r) =>
  UserId ->
  Sem r ()
deleteUserNoVerify uid = do
  enqueueUserDeletion uid

deleteUsersNoVerify ::
  (Member DeleteQueue r) =>
  [UserId] ->
  AppT r ()
deleteUsersNoVerify uids = do
  liftSem $ for_ uids deleteUserNoVerify
  void $ Prom.addCounter enqueueMultiDeleteCounter (fromIntegral $ length uids)
  Prom.incCounter enqueueMultiDeleteCallsCounter

{-# NOINLINE enqueueMultiDeleteCounter #-}
enqueueMultiDeleteCounter :: Prom.Counter
enqueueMultiDeleteCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user_enqueue_multi_delete_total",
          Prom.metricHelp = "Number of users enqueued to be deleted"
        }

{-# NOINLINE enqueueMultiDeleteCallsCounter #-}
enqueueMultiDeleteCallsCounter :: Prom.Counter
enqueueMultiDeleteCallsCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user_enqueue_multi_delete_calls_total",
          Prom.metricHelp = "Number of users enqueued to be deleted"
        }

getLegalHoldStatus ::
  ( Member GalleyAPIAccess r,
    Member UserSubsystem r
  ) =>
  Local UserId ->
  AppT r (Maybe UserLegalHoldStatus)
getLegalHoldStatus uid =
  liftSem $
    traverse getLegalHoldStatus'
      =<< User.getLocalAccountBy NoPendingInvitations uid

getLegalHoldStatus' ::
  (Member GalleyAPIAccess r) =>
  User ->
  Sem r UserLegalHoldStatus
getLegalHoldStatus' user =
  case userTeam user of
    Nothing -> pure defUserLegalHoldStatus
    Just tid -> do
      teamMember <- GalleyAPIAccess.getTeamMember (userId user) tid
      pure $ maybe defUserLegalHoldStatus (^. legalHoldStatus) teamMember

isBlacklisted :: (Member BlockListStore r) => EmailAddress -> AppT r Bool
isBlacklisted email = do
  let uk = mkEmailKey email
  liftSem $ BlockListStore.exists uk

blacklistInsert :: (Member BlockListStore r) => EmailAddress -> AppT r ()
blacklistInsert email = do
  let uk = mkEmailKey email
  liftSem $ BlockListStore.insert uk

blacklistDelete :: (Member BlockListStore r) => EmailAddress -> AppT r ()
blacklistDelete email = do
  let uk = mkEmailKey email
  liftSem $ BlockListStore.delete uk
