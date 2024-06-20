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
    createUser,
    createUserSpar,
    createUserInviteViaScim,
    checkRestrictedUserCreation,
    changeSelfEmail,
    changeEmail,
    changePhone,
    CheckHandleResp (..),
    checkHandle,
    lookupHandle,
    changeManagedBy,
    changeAccountStatus,
    changeSingleAccountStatus,
    Data.lookupAccounts,
    Data.lookupAccount,
    lookupAccountsByIdentity,
    lookupProfilesV3,
    getLegalHoldStatus,
    Data.lookupName,
    Data.lookupLocale,
    Data.lookupUser,
    Data.lookupRichInfo,
    Data.lookupRichInfoMultiUsers,
    removeEmail,
    removePhone,
    revokeIdentity,
    deleteUserNoVerify,
    deleteUsersNoVerify,
    deleteSelfUser,
    verifyDeleteUser,
    ensureAccountDeleted,
    deleteAccount,
    checkHandles,
    isBlacklistedHandle,
    Data.reauthenticate,

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

    -- * Phone Prefix blocking
    phonePrefixGet,
    phonePrefixDelete,
    phonePrefixInsert,

    -- * Utilities
    fetchUserIdentity,
  )
where

import Brig.API.Error qualified as Error
import Brig.API.Handler qualified as API (UserNotAllowedToJoinTeam (..))
import Brig.API.Types
import Brig.API.Util
import Brig.App
import Brig.Code qualified as Code
import Brig.Data.Activation (ActivationEvent (..), activationErrorToRegisterError)
import Brig.Data.Activation qualified as Data
import Brig.Data.Client qualified as Data
import Brig.Data.Connection (countConnections)
import Brig.Data.Connection qualified as Data
import Brig.Data.Properties qualified as Data
import Brig.Data.User
import Brig.Data.User qualified as Data
import Brig.Effects.BlacklistPhonePrefixStore (BlacklistPhonePrefixStore)
import Brig.Effects.BlacklistPhonePrefixStore qualified as BlacklistPhonePrefixStore
import Brig.Effects.BlacklistStore (BlacklistStore)
import Brig.Effects.BlacklistStore qualified as BlacklistStore
import Brig.Effects.ConnectionStore (ConnectionStore)
import Brig.Effects.UserPendingActivationStore (UserPendingActivation (..), UserPendingActivationStore)
import Brig.Effects.UserPendingActivationStore qualified as UserPendingActivationStore
import Brig.IO.Intra qualified as Intra
import Brig.Options hiding (Timeout, internalEvents)
import Brig.Team.DB qualified as Team
import Brig.Types.Activation (ActivationPair)
import Brig.Types.Connection
import Brig.Types.Intra
import Brig.User.Auth.Cookie qualified as Auth
import Brig.User.Email
import Brig.User.Phone
import Brig.User.Search.Index (reindex)
import Brig.User.Search.TeamSize qualified as TeamSize
import Cassandra hiding (Set)
import Control.Error
import Control.Lens (view, (^.))
import Control.Monad.Catch
import Data.ByteString.Conversion
import Data.Code
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
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.UUID.V4 (nextRandom)
import Imports
import Network.Wai.Utilities
import Polysemy
import Polysemy.Input (Input)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import Prometheus qualified as Prom
import System.Logger.Message
import UnliftIO.Async (mapConcurrently_)
import Wire.API.Connection
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Federation.Error
import Wire.API.Password
import Wire.API.Routes.Internal.Galley.TeamsIntra qualified as Team
import Wire.API.Team hiding (newTeam)
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.Invitation qualified as Team
import Wire.API.Team.Member (legalHoldStatus)
import Wire.API.Team.Role
import Wire.API.Team.Size
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Client
import Wire.API.User.RichInfo
import Wire.API.UserEvent
import Wire.AuthenticationSubsystem (AuthenticationSubsystem, internalLookupPasswordResetCode)
import Wire.DeleteQueue
import Wire.GalleyAPIAccess as GalleyAPIAccess
import Wire.NotificationSubsystem
import Wire.Sem.Concurrency
import Wire.Sem.Paging.Cassandra (InternalPaging)
import Wire.UserKeyStore
import Wire.UserStore
import Wire.UserSubsystem as User
import Wire.UserSubsystem.HandleBlacklist

-------------------------------------------------------------------------------
-- Create User

data IdentityError
  = IdentityErrorBlacklistedEmail
  | IdentityErrorBlacklistedPhone
  | IdentityErrorUserKeyExists

identityErrorToRegisterError :: IdentityError -> RegisterError
identityErrorToRegisterError = \case
  IdentityErrorBlacklistedEmail -> RegisterErrorBlacklistedEmail
  IdentityErrorBlacklistedPhone -> RegisterErrorBlacklistedPhone
  IdentityErrorUserKeyExists -> RegisterErrorUserKeyExists

identityErrorToBrigError :: IdentityError -> Error.Error
identityErrorToBrigError = \case
  IdentityErrorBlacklistedEmail -> Error.StdError $ errorToWai @'E.BlacklistedEmail
  IdentityErrorBlacklistedPhone -> Error.StdError $ errorToWai @'E.BlacklistedPhone
  IdentityErrorUserKeyExists -> Error.StdError $ errorToWai @'E.UserKeyExists

verifyUniquenessAndCheckBlacklist ::
  (Member BlacklistStore r, Member UserKeyStore r) =>
  UserKey ->
  ExceptT IdentityError (AppT r) ()
verifyUniquenessAndCheckBlacklist uk = do
  checkKey Nothing uk
  blacklisted <- lift $ liftSem $ BlacklistStore.exists uk
  when blacklisted $
    throwE (foldKey (const IdentityErrorBlacklistedEmail) (const IdentityErrorBlacklistedPhone) uk)
  where
    checkKey u k = do
      av <- lift $ liftSem $ keyAvailable k u
      unless av $
        throwE IdentityErrorUserKeyExists

createUserSpar ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member UserSubsystem r,
    Member (ConnectionStore InternalPaging) r
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
    (account, pw) <- wrapClient $ newAccount new' Nothing (Just tid) handle'

    let uid = userId (accountUser account)

    -- FUTUREWORK: make this transactional if possible
    wrapClient $ Data.insertAccount account Nothing pw False
    case unRichInfo <$> newUserSparRichInfo new of
      Just richInfo -> wrapClient $ Data.updateRichInfo uid richInfo
      Nothing -> pure () -- Nothing to do
    liftSem $ GalleyAPIAccess.createSelfConv uid
    liftSem $ Intra.onUserEvent uid Nothing (UserCreated (accountUser account))

    pure account

  -- Add to team
  userTeam <- withExceptT CreateUserSparRegistrationError $ addUserToTeamSSO account tid (SSOIdentity ident Nothing Nothing) (newUserSparRole new)

  -- Set up feature flags
  luid <- lift $ ensureLocal (userQualifiedId (accountUser account))
  lift $ initAccountFeatureConfig (tUnqualified luid)

  -- Set handle
  lift $ updateHandle' luid handle'

  pure $! CreateUserResult account Nothing Nothing (Just userTeam)
  where
    updateHandle' :: Local UserId -> Maybe Handle -> AppT r ()
    updateHandle' _ Nothing = pure ()
    updateHandle' luid (Just h) =
      liftSem $ User.updateHandle luid Nothing UpdateOriginScim (fromHandle h)

    addUserToTeamSSO :: UserAccount -> TeamId -> UserIdentity -> Role -> ExceptT RegisterError (AppT r) CreateUserTeam
    addUserToTeamSSO account tid ident role = do
      let uid = userId (accountUser account)
      added <- lift $ liftSem $ GalleyAPIAccess.addTeamMember uid tid (Nothing, role)
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

-- docs/reference/user/registration.md {#RefRegistration}
createUser ::
  forall r p.
  ( Member BlacklistStore r,
    Member GalleyAPIAccess r,
    Member (UserPendingActivationStore p) r,
    Member UserKeyStore r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  NewUser ->
  ExceptT RegisterError (AppT r) CreateUserResult
createUser new = do
  (email, phone) <- validateEmailAndPhone new

  -- get invitation and existing account
  (mNewTeamUser, teamInvitation, tid) <-
    case newUserTeam new of
      Just (NewTeamMember i) -> do
        mbTeamInv <- findTeamInvitation (userEmailKey <$> email) i
        case mbTeamInv of
          Just (inv, info, tid) ->
            pure (Nothing, Just (inv, info), Just tid)
          Nothing ->
            pure (Nothing, Nothing, Nothing)
      Just (NewTeamCreator t) -> do
        (Just t,Nothing,) <$> (Just . Id <$> liftIO nextRandom)
      Just (NewTeamMemberSSO tid) ->
        pure (Nothing, Nothing, Just tid)
      Nothing ->
        pure (Nothing, Nothing, Nothing)
  let mbInv = Team.inInvitation . fst <$> teamInvitation
  mbExistingAccount <- lift $ join <$> for mbInv (\(Id uuid) -> wrapClient $ Data.lookupAccount (Id uuid))

  let (new', mbHandle) = case mbExistingAccount of
        Nothing ->
          ( new {newUserIdentity = newIdentity email phone (newUserSSOId new)},
            Nothing
          )
        Just existingAccount ->
          let existingUser = accountUser existingAccount
              mbSSOid =
                case (teamInvitation, email, userManagedBy existingUser) of
                  -- isJust teamInvitation And ManagedByScim implies that the
                  -- user invitation has been generated by SCIM and there is no IdP
                  (Just _, Just em, ManagedByScim) ->
                    Just $ UserScimExternalId (fromEmail em)
                  _ -> newUserSSOId new
           in ( new
                  { newUserManagedBy = Just (userManagedBy existingUser),
                    newUserIdentity = newIdentity email phone mbSSOid
                  },
                userHandle existingUser
              )

  -- Create account
  account <- lift $ do
    (account, pw) <- wrapClient $ newAccount new' mbInv tid mbHandle

    let uid = userId (accountUser account)
    liftSem $ do
      Log.debug $ field "user" (toByteString uid) . field "action" (val "User.createUser")
      Log.info $ field "user" (toByteString uid) . msg (val "Creating user")

    wrapClient $ Data.insertAccount account Nothing pw False
    liftSem $ GalleyAPIAccess.createSelfConv uid
    liftSem $ Intra.onUserEvent uid Nothing (UserCreated (accountUser account))

    pure account

  let uid = userId (accountUser account)

  createUserTeam <- do
    activatedTeam <- lift $ do
      case (tid, mNewTeamUser) of
        (Just tid', Just newTeamUser) -> do
          liftSem $ GalleyAPIAccess.createTeam uid (bnuTeam newTeamUser) tid'
          let activating = isJust (newUserEmailCode new)
              BindingNewTeam newTeam = newTeamUser.bnuTeam
          pure $
            if activating
              then Just $ CreateUserTeam tid' (fromRange (newTeam ^. newTeamName))
              else Nothing
        _ -> pure Nothing

    joinedTeamInvite <- case teamInvitation of
      Just (inv, invInfo) -> do
        let em = Team.inInviteeEmail inv
        acceptTeamInvitation account inv invInfo (userEmailKey em) (EmailIdentity em)
        Team.TeamName nm <- lift $ liftSem $ GalleyAPIAccess.getTeamName (Team.inTeam inv)
        pure (Just $ CreateUserTeam (Team.inTeam inv) nm)
      Nothing -> pure Nothing

    joinedTeamSSO <- case (newUserIdentity new', tid) of
      (Just ident@(SSOIdentity (UserSSOId _) _ _), Just tid') -> Just <$> addUserToTeamSSO account tid' ident
      _ -> pure Nothing

    pure (activatedTeam <|> joinedTeamInvite <|> joinedTeamSSO)

  edata <-
    if isJust teamInvitation
      then pure Nothing
      else handleEmailActivation email uid mNewTeamUser

  pdata <- handlePhoneActivation phone uid

  lift $ initAccountFeatureConfig uid

  pure $! CreateUserResult account edata pdata createUserTeam
  where
    -- NOTE: all functions in the where block don't use any arguments of createUser

    validateEmailAndPhone :: NewUser -> ExceptT RegisterError (AppT r) (Maybe Email, Maybe Phone)
    validateEmailAndPhone newUser = do
      -- Validate e-mail
      email <- for (newUserEmail newUser) $ \e ->
        either
          (const $ throwE RegisterErrorInvalidEmail)
          pure
          (validateEmail e)

      -- Validate phone
      phone <- for (newUserPhone newUser) $ \p ->
        maybe
          (throwE RegisterErrorInvalidPhone)
          pure
          =<< lift (wrapClient $ validatePhone p)

      for_ (catMaybes [userEmailKey <$> email, userPhoneKey <$> phone]) $ \k ->
        verifyUniquenessAndCheckBlacklist k !>> identityErrorToRegisterError

      pure (email, phone)

    findTeamInvitation :: Maybe UserKey -> InvitationCode -> ExceptT RegisterError (AppT r) (Maybe (Team.Invitation, Team.InvitationInfo, TeamId))
    findTeamInvitation Nothing _ = throwE RegisterErrorMissingIdentity
    findTeamInvitation (Just e) c =
      lift (wrapClient $ Team.lookupInvitationInfo c) >>= \case
        Just ii -> do
          inv <- lift . wrapClient $ Team.lookupInvitation HideInvitationUrl (Team.iiTeam ii) (Team.iiInvId ii)
          case (inv, Team.inInviteeEmail <$> inv) of
            (Just invite, Just em)
              | e == userEmailKey em -> do
                  _ <- ensureMemberCanJoin (Team.iiTeam ii)
                  pure $ Just (invite, ii, Team.iiTeam ii)
            _ -> throwE RegisterErrorInvalidInvitationCode
        Nothing -> throwE RegisterErrorInvalidInvitationCode

    ensureMemberCanJoin :: TeamId -> ExceptT RegisterError (AppT r) ()
    ensureMemberCanJoin tid = do
      maxSize <- fromIntegral . setMaxTeamSize <$> view settings
      (TeamSize teamSize) <- TeamSize.teamSize tid
      when (teamSize >= maxSize) $
        throwE RegisterErrorTooManyTeamMembers
      -- FUTUREWORK: The above can easily be done/tested in the intra call.
      --             Remove after the next release.
      canAdd <- lift $ liftSem $ GalleyAPIAccess.checkUserCanJoinTeam tid
      case canAdd of
        Just e -> throwM $ API.UserNotAllowedToJoinTeam e
        Nothing -> pure ()

    acceptTeamInvitation ::
      UserAccount ->
      Team.Invitation ->
      Team.InvitationInfo ->
      UserKey ->
      UserIdentity ->
      ExceptT RegisterError (AppT r) ()
    acceptTeamInvitation account inv ii uk ident = do
      let uid = userId (accountUser account)
      ok <- lift $ liftSem $ claimKey uk uid
      unless ok $
        throwE RegisterErrorUserKeyExists
      let minvmeta :: (Maybe (UserId, UTCTimeMillis), Role)
          minvmeta = ((,inCreatedAt inv) <$> inCreatedBy inv, Team.inRole inv)
      added <- lift $ liftSem $ GalleyAPIAccess.addTeamMember uid (Team.iiTeam ii) minvmeta
      unless added $
        throwE RegisterErrorTooManyTeamMembers
      lift $ do
        wrapClient $ activateUser uid ident -- ('insertAccount' sets column activated to False; here it is set to True.)
        void $ onActivated (AccountActivated account)
        liftSem $
          Log.info $
            field "user" (toByteString uid)
              . field "team" (toByteString $ Team.iiTeam ii)
              . msg (val "Accepting invitation")
        liftSem $ UserPendingActivationStore.remove uid
        wrapClient $ do
          Team.deleteInvitation (Team.inTeam inv) (Team.inInvitation inv)

    addUserToTeamSSO :: UserAccount -> TeamId -> UserIdentity -> ExceptT RegisterError (AppT r) CreateUserTeam
    addUserToTeamSSO account tid ident = do
      let uid = userId (accountUser account)
      added <- lift $ liftSem $ GalleyAPIAccess.addTeamMember uid tid (Nothing, defaultRole)
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
    handleEmailActivation :: Maybe Email -> UserId -> Maybe BindingNewTeamUser -> ExceptT RegisterError (AppT r) (Maybe Activation)
    handleEmailActivation email uid newTeam = do
      fmap join . for (userEmailKey <$> email) $ \ek -> case newUserEmailCode new of
        Nothing -> do
          timeout <- setActivationTimeout <$> view settings
          edata <- lift . wrapClient $ Data.newActivation ek timeout (Just uid)
          lift . liftSem . Log.info $
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

    -- Handle phone activation (deprecated, see #RefRegistrationNoPreverification in /docs/reference/user/registration.md)
    handlePhoneActivation :: Maybe Phone -> UserId -> ExceptT RegisterError (AppT r) (Maybe Activation)
    handlePhoneActivation phone uid = do
      fmap join . for (userPhoneKey <$> phone) $ \pk -> case newUserPhoneCode new of
        Nothing -> do
          timeout <- setActivationTimeout <$> view settings
          pdata <- lift . wrapClient $ Data.newActivation pk timeout (Just uid)
          lift . liftSem . Log.info $
            field "user" (toByteString uid)
              . field "activation.key" (toByteString $ activationKey pdata)
              . msg (val "Created phone activation key/code pair")
          pure $ Just pdata
        Just c -> do
          ak <- liftIO $ Data.mkActivationKey pk
          void $ activate (ActivateKey ak) c (Just uid) !>> activationErrorToRegisterError
          pure Nothing

initAccountFeatureConfig :: UserId -> (AppT r) ()
initAccountFeatureConfig uid = do
  mbCciDefNew <- view (settings . getAfcConferenceCallingDefNewMaybe)
  forM_ (forgetLock <$> mbCciDefNew) $ wrapClient . Data.updateFeatureConferenceCalling uid . Just

-- | 'createUser' is becoming hard to maintain, and instead of adding more case distinctions
-- all over the place there, we add a new function that handles just the one new flow where
-- users are invited to the team via scim.
createUserInviteViaScim ::
  ( Member BlacklistStore r,
    Member UserKeyStore r,
    Member (UserPendingActivationStore p) r,
    Member TinyLog r
  ) =>
  NewUserScimInvitation ->
  ExceptT Error.Error (AppT r) UserAccount
createUserInviteViaScim (NewUserScimInvitation tid uid loc name rawEmail _) = do
  email <- either (const . throwE . Error.StdError $ errorToWai @'E.InvalidEmail) pure (validateEmail rawEmail)
  let emKey = userEmailKey email
  verifyUniquenessAndCheckBlacklist emKey !>> identityErrorToBrigError
  account <- lift . wrapClient $ newAccountInviteViaScim uid tid loc name email
  lift . liftSem . Log.debug $ field "user" (toByteString . userId . accountUser $ account) . field "action" (val "User.createUserInviteViaScim")

  -- add the expiry table entry first!  (if brig creates an account, and then crashes before
  -- creating the expiry table entry, gc will miss user data.)
  expiresAt <- do
    ttl <- setTeamInvitationTimeout <$> view settings
    now <- liftIO =<< view currentTime
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
  restrictPlease <- lift . asks $ fromMaybe False . setRestrictUserCreation . view settings
  when
    ( restrictPlease
        && not (isNewUserTeamMember new)
        && not (isNewUserEphemeral new)
    )
    $ throwE RegisterErrorUserCreationRestricted

-------------------------------------------------------------------------------
-- Update ManagedBy

changeManagedBy ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  ConnId ->
  ManagedByUpdate ->
  (AppT r) ()
changeManagedBy uid conn (ManagedByUpdate mb) = do
  wrapClient $ Data.updateManagedBy uid mb
  liftSem $ Intra.onUserEvent uid (Just conn) (managedByUpdate uid mb)

-------------------------------------------------------------------------------
-- Change Email

-- | Call 'changeEmail' and process result: if email changes to itself, succeed, if not, send
-- validation email.
changeSelfEmail :: (Member BlacklistStore r, Member UserKeyStore r) => UserId -> Email -> UpdateOriginType -> ExceptT Error.Error (AppT r) ChangeEmailResponse
changeSelfEmail u email allowScim = do
  changeEmail u email allowScim !>> Error.changeEmailError >>= \case
    ChangeEmailIdempotent ->
      pure ChangeEmailResponseIdempotent
    ChangeEmailNeedsActivation (usr, adata, en) -> lift $ do
      sendOutEmail usr adata en
      wrapClient $ Data.updateEmailUnvalidated u email
      wrapClient $ reindex u
      pure ChangeEmailResponseNeedsActivation
  where
    sendOutEmail usr adata en = do
      sendActivationMail
        en
        (userDisplayName usr)
        (activationKey adata, activationCode adata)
        (Just (userLocale usr))
        (userIdentity usr)

-- | Prepare changing the email (checking a number of invariants).
changeEmail :: (Member BlacklistStore r, Member UserKeyStore r) => UserId -> Email -> UpdateOriginType -> ExceptT ChangeEmailError (AppT r) ChangeEmailResult
changeEmail u email updateOrigin = do
  em <-
    either
      (throwE . InvalidNewEmail email)
      pure
      (validateEmail email)
  let ek = userEmailKey em
  blacklisted <- lift . liftSem $ BlacklistStore.exists ek
  when blacklisted $
    throwE (ChangeBlacklistedEmail email)
  available <- lift $ liftSem $ keyAvailable ek (Just u)
  unless available $
    throwE $
      EmailExists email
  usr <- maybe (throwM $ UserProfileNotFound u) pure =<< lift (wrapClient $ Data.lookupUser WithPendingInvitations u)
  case emailIdentity =<< userIdentity usr of
    -- The user already has an email address and the new one is exactly the same
    Just current | current == em -> pure ChangeEmailIdempotent
    _ -> do
      unless (userManagedBy usr /= ManagedByScim || updateOrigin == UpdateOriginScim) $
        throwE EmailManagedByScim
      timeout <- setActivationTimeout <$> view settings
      act <- lift . wrapClient $ Data.newActivation ek timeout (Just u)
      pure $ ChangeEmailNeedsActivation (usr, act, em)

-------------------------------------------------------------------------------
-- Change Phone

changePhone ::
  ( Member BlacklistStore r,
    Member UserKeyStore r,
    Member BlacklistPhonePrefixStore r
  ) =>
  UserId ->
  Phone ->
  ExceptT ChangePhoneError (AppT r) (Activation, Phone)
changePhone u phone = do
  canonical <-
    maybe
      (throwE InvalidNewPhone)
      pure
      =<< lift (wrapClient $ validatePhone phone)
  let pk = userPhoneKey canonical
  available <- lift $ liftSem $ keyAvailable pk (Just u)
  unless available $
    throwE PhoneExists
  timeout <- setActivationTimeout <$> view settings
  blacklisted <- lift . liftSem $ BlacklistStore.exists pk
  when blacklisted $
    throwE BlacklistedNewPhone
  -- check if any prefixes of this phone number are blocked
  prefixExcluded <- lift . liftSem $ BlacklistPhonePrefixStore.existsAny canonical
  when prefixExcluded $
    throwE BlacklistedNewPhone
  act <- lift . wrapClient $ Data.newActivation pk timeout (Just u)
  pure (act, canonical)

-------------------------------------------------------------------------------
-- Remove Email

removeEmail ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member UserKeyStore r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r,
    Member UserSubsystem r
  ) =>
  UserId ->
  ConnId ->
  ExceptT RemoveIdentityError (AppT r) ()
removeEmail uid conn = do
  ident <- lift $ fetchUserIdentity uid
  case ident of
    Just (FullIdentity e _) -> lift $ do
      liftSem $ deleteKey $ userEmailKey e
      wrapClient $ Data.deleteEmail uid
      liftSem $ Intra.onUserEvent uid (Just conn) (emailRemoved uid e)
    Just _ -> throwE LastIdentity
    Nothing -> throwE NoIdentity

-------------------------------------------------------------------------------
-- Remove Phone

removePhone ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member UserKeyStore r,
    Member UserStore r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r,
    Member UserSubsystem r
  ) =>
  UserId ->
  ConnId ->
  ExceptT RemoveIdentityError (AppT r) ()
removePhone uid conn = do
  ident <- lift $ fetchUserIdentity uid
  case ident of
    Just (FullIdentity _ p) -> do
      pw <- lift $ liftSem $ lookupPassword uid
      unless (isJust pw) $
        throwE NoPassword
      lift $ do
        liftSem $ deleteKey $ userPhoneKey p
        wrapClient $ Data.deletePhone uid
        liftSem $ Intra.onUserEvent uid (Just conn) (phoneRemoved uid p)
    Just _ -> throwE LastIdentity
    Nothing -> throwE NoIdentity

-------------------------------------------------------------------------------
-- Forcefully revoke a verified identity

revokeIdentity ::
  forall r.
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member UserKeyStore r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r,
    Member UserSubsystem r
  ) =>
  Either Email Phone ->
  AppT r ()
revokeIdentity key = do
  let uk = either userEmailKey userPhoneKey key
  mu <- liftSem $ lookupKey uk
  case mu of
    Nothing -> pure ()
    Just u ->
      fetchUserIdentity u >>= \case
        Just (FullIdentity _ _) -> revokeKey u uk
        Just (EmailIdentity e) | Left e == key -> do
          revokeKey u uk
          wrapClient $ Data.deactivateUser u
        Just (PhoneIdentity p) | Right p == key -> do
          revokeKey u uk
          wrapClient $ Data.deactivateUser u
        _ -> pure ()
  where
    revokeKey :: UserId -> UserKey -> AppT r ()
    revokeKey u uk = do
      liftSem $ deleteKey uk
      wrapClient $
        foldKey
          (\(_ :: Email) -> Data.deleteEmail u)
          (\(_ :: Phone) -> Data.deletePhone u)
          uk
      liftSem $
        Intra.onUserEvent u Nothing $
          foldKey
            (emailRemoved u)
            (phoneRemoved u)
            uk

-------------------------------------------------------------------------------
-- Change Account Status

changeAccountStatus ::
  forall r.
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Concurrency 'Unsafe) r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
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
      Intra.onUserEvent u Nothing (ev u)

changeSingleAccountStatus ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  AccountStatus ->
  ExceptT AccountStatusError (AppT r) ()
changeSingleAccountStatus uid status = do
  unlessM (wrapClientE $ Data.userExists uid) $ throwE AccountNotFound
  ev <- mkUserEvent (List1.singleton uid) status
  lift $ do
    wrapClient $ Data.updateStatus uid status
    liftSem $ Intra.onUserEvent uid Nothing (ev uid)

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
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
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
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  ActivationTarget ->
  ActivationCode ->
  -- | The user for whom to activate the key.
  Maybe UserId ->
  -- | Potential currency update.
  -- ^ TODO: to be removed once billing supports currency changes after team creation
  Maybe Currency.Alpha ->
  ExceptT ActivationError (AppT r) ActivationResult
activateWithCurrency tgt code usr cur = do
  key <- wrapClientE $ mkActivationKey tgt
  lift . liftSem . Log.info $
    field "activation.key" (toByteString key)
      . field "activation.code" (toByteString code)
      . msg (val "Activating")
  event <- wrapClientE $ Data.activateKey key code usr
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
  ExceptT ActivationError m ()
preverify tgt code = do
  key <- mkActivationKey tgt
  void $ Data.verifyCode key code

onActivated ::
  ( Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  ActivationEvent ->
  (AppT r) (UserId, Maybe UserIdentity, Bool)
onActivated (AccountActivated account) = liftSem $ do
  let uid = userId (accountUser account)
  Log.debug $ field "user" (toByteString uid) . field "action" (val "User.onActivated")
  Log.info $ field "user" (toByteString uid) . msg (val "User activated")
  Intra.onUserEvent uid Nothing $ UserActivated (accountUser account)
  pure (uid, userIdentity (accountUser account), True)
onActivated (EmailActivated uid email) = do
  liftSem $ Intra.onUserEvent uid Nothing (emailUpdated uid email)
  wrapHttpClient $ Data.deleteEmailUnvalidated uid
  pure (uid, Just (EmailIdentity email), False)
onActivated (PhoneActivated uid phone) = do
  liftSem $ Intra.onUserEvent uid Nothing (phoneUpdated uid phone)
  pure (uid, Just (PhoneIdentity phone), False)

-- docs/reference/user/activation.md {#RefActivationRequest}
sendActivationCode ::
  ( Member BlacklistStore r,
    Member BlacklistPhonePrefixStore r,
    Member UserKeyStore r,
    Member GalleyAPIAccess r
  ) =>
  Either Email Phone ->
  Maybe Locale ->
  Bool ->
  ExceptT SendActivationCodeError (AppT r) ()
sendActivationCode emailOrPhone loc call = case emailOrPhone of
  Left email -> do
    ek <-
      either
        (const . throwE . InvalidRecipient $ userEmailKey email)
        (pure . userEmailKey)
        (validateEmail email)
    exists <- lift $ liftSem $ isJust <$> lookupKey ek
    when exists $
      throwE $
        UserKeyInUse ek
    blacklisted <- lift . liftSem $ BlacklistStore.exists ek
    when blacklisted $
      throwE (ActivationBlacklistedUserKey ek)
    uc <- lift . wrapClient $ Data.lookupActivationCode ek
    case uc of
      Nothing -> sendVerificationEmail ek Nothing -- Fresh code request, no user
      Just (Nothing, c) -> sendVerificationEmail ek (Just c) -- Re-requesting existing code
      Just (Just uid, c) -> sendActivationEmail ek c uid -- User re-requesting activation
  Right phone -> do
    -- validatePhone returns the canonical E.164 phone number format
    canonical <-
      maybe
        (throwE $ InvalidRecipient (userPhoneKey phone))
        pure
        =<< lift (wrapClient $ validatePhone phone)
    let pk = userPhoneKey canonical
    exists <- lift $ liftSem $ isJust <$> lookupKey pk
    when exists $
      throwE $
        UserKeyInUse pk
    blacklisted <- lift . liftSem $ BlacklistStore.exists pk
    when blacklisted $
      throwE (ActivationBlacklistedUserKey pk)
    -- check if any prefixes of this phone number are blocked
    prefixExcluded <- lift . liftSem $ BlacklistPhonePrefixStore.existsAny canonical
    when prefixExcluded $
      throwE (ActivationBlacklistedUserKey pk)
    c <- lift . wrapClient $ fmap snd <$> Data.lookupActivationCode pk
    p <- wrapClientE $ mkPair pk c Nothing
    void . lift . wrapHttp $ forPhoneKey pk $ \ph ->
      if call
        then sendActivationCall ph p loc
        else sendActivationSms ph p loc
  where
    notFound = throwM . UserDisplayNameNotFound
    mkPair k c u = do
      timeout <- setActivationTimeout <$> view settings
      case c of
        Just c' -> liftIO $ (,c') <$> Data.mkActivationKey k
        Nothing -> lift $ do
          dat <- Data.newActivation k timeout u
          pure (activationKey dat, activationCode dat)
    sendVerificationEmail ek uc = do
      p <- wrapClientE $ mkPair ek uc Nothing
      void . forEmailKey ek $ \em ->
        lift $
          sendVerificationMail em p loc
    sendActivationEmail ek uc uid = do
      -- FUTUREWORK(fisx): we allow for 'PendingInvitations' here, but I'm not sure this
      -- top-level function isn't another piece of a deprecated onboarding flow?
      u <- maybe (notFound uid) pure =<< lift (wrapClient $ Data.lookupUser WithPendingInvitations uid)
      p <- wrapClientE $ mkPair ek (Just uc) (Just uid)
      let ident = userIdentity u
          name = userDisplayName u
          loc' = loc <|> Just (userLocale u)
      void . forEmailKey ek $ \em -> lift $ do
        -- Get user's team, if any.
        mbTeam <- mapM (fmap Team.tdTeam . liftSem . GalleyAPIAccess.getTeam) (userTeam u)
        -- Depending on whether the user is a team creator, send either
        -- a team activation email or a regular email. Note that we
        -- don't have to check if the team is binding because if the
        -- user has 'userTeam' set, it must be binding.
        case mbTeam of
          Just team
            | team ^. teamCreator == uid ->
                sendTeamActivationMail em name p loc' (team ^. teamName)
          _otherwise ->
            sendActivationMail em name p loc' ident

mkActivationKey :: (MonadClient m, MonadReader Env m) => ActivationTarget -> ExceptT ActivationError m ActivationKey
mkActivationKey (ActivateKey k) = pure k
mkActivationKey (ActivateEmail e) = do
  ek <-
    either
      (throwE . InvalidActivationEmail e)
      (pure . userEmailKey)
      (validateEmail e)
  liftIO $ Data.mkActivationKey ek
mkActivationKey (ActivatePhone p) = do
  pk <-
    maybe
      (throwE $ InvalidActivationPhone p)
      (pure . userPhoneKey)
      =<< lift (validatePhone p)
  liftIO $ Data.mkActivationKey pk

-------------------------------------------------------------------------------
-- Password Management

changePassword :: (Member UserStore r) => UserId -> PasswordChange -> ExceptT ChangePasswordError (AppT r) ()
changePassword uid cp = do
  activated <- lift $ liftSem $ isActivated uid
  unless activated $
    throwE ChangePasswordNoIdentity
  currpw <- lift $ liftSem $ lookupPassword uid
  let newpw = cpNewPassword cp
  hashedNewPw <- mkSafePasswordArgon2id newpw
  case (currpw, cpOldPassword cp) of
    (Nothing, _) -> lift . liftSem $ updatePassword uid hashedNewPw
    (Just _, Nothing) -> throwE InvalidCurrentPassword
    (Just pw, Just pw') -> do
      -- We are updating the pwd here anyway, so we don't care about the pwd status
      unless (verifyPassword pw' pw) $
        throwE InvalidCurrentPassword
      when (verifyPassword newpw pw) $
        throwE ChangePasswordMustDiffer
      lift $ liftSem (updatePassword uid hashedNewPw) >> wrapClient (Auth.revokeAllCookies uid)

-------------------------------------------------------------------------------
-- User Deletion

-- | Initiate validation of a user's delete request.  Called via @delete /self@.  Users with an
-- 'UserSSOId' can still do this if they also have an 'Email', 'Phone', and/or password.  Otherwise,
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
    Member (Input (Local ())) r,
    Member UserStore r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserId ->
  Maybe PlainTextPassword6 ->
  ExceptT DeleteUserError (AppT r) (Maybe Timeout)
deleteSelfUser uid pwd = do
  account <- lift . wrapClient $ Data.lookupAccount uid
  case account of
    Nothing -> throwE DeleteUserInvalid
    Just a -> case accountStatus a of
      Deleted -> pure Nothing
      Suspended -> ensureNotOwner a >> go a
      Active -> ensureNotOwner a >> go a
      Ephemeral -> go a
      PendingInvitation -> go a
  where
    ensureNotOwner :: UserAccount -> ExceptT DeleteUserError (AppT r) ()
    ensureNotOwner acc = do
      case userTeam $ accountUser acc of
        Nothing -> pure ()
        Just tid -> do
          isOwner <- lift $ liftSem $ GalleyAPIAccess.memberIsTeamOwner tid uid
          when isOwner $ throwE DeleteUserOwnerDeletingSelf
    go a = maybe (byIdentity a) (byPassword a) pwd
    getEmailOrPhone :: UserIdentity -> Maybe (Either Email Phone)
    getEmailOrPhone (FullIdentity e _) = Just $ Left e
    getEmailOrPhone (EmailIdentity e) = Just $ Left e
    getEmailOrPhone (SSOIdentity _ (Just e) _) = Just $ Left e
    getEmailOrPhone (PhoneIdentity p) = Just $ Right p
    getEmailOrPhone (SSOIdentity _ _ (Just p)) = Just $ Right p
    getEmailOrPhone (SSOIdentity _ Nothing Nothing) = Nothing
    byIdentity a = case getEmailOrPhone =<< userIdentity (accountUser a) of
      Just emailOrPhone -> sendCode a emailOrPhone
      Nothing -> case pwd of
        Just _ -> throwE DeleteUserMissingPassword
        Nothing -> lift . liftSem $ deleteAccount a >> pure Nothing
    byPassword a pw = do
      lift . liftSem . Log.info $
        field "user" (toByteString uid)
          . msg (val "Attempting account deletion with a password")
      actual <- lift $ liftSem $ lookupPassword uid
      case actual of
        Nothing -> throwE DeleteUserInvalidPassword
        Just p -> do
          -- We're deleting a user, no sense in updating their pwd, so we ignore pwd status
          unless (verifyPassword pw p) $
            throwE DeleteUserInvalidPassword
          lift . liftSem $ deleteAccount a >> pure Nothing
    sendCode a target = do
      gen <- Code.mkGen (either Code.ForEmail Code.ForPhone target)
      pending <- lift . wrapClient $ Code.lookup (Code.genKey gen) Code.AccountDeletion
      case pending of
        Just c -> throwE $! DeleteUserPendingCode (Code.codeTTL c)
        Nothing -> do
          lift . liftSem . Log.info $
            field "user" (toByteString uid)
              . msg (val "Sending verification code for account deletion")
          c <-
            Code.generate
              gen
              Code.AccountDeletion
              (Code.Retries 3)
              (Code.Timeout 600)
              (Just (toUUID uid))
          tryInsertVerificationCode c DeleteUserVerificationCodeThrottled
          let k = Code.codeKey c
          let v = Code.codeValue c
          let l = userLocale (accountUser a)
          let n = userDisplayName (accountUser a)
          either
            (\e -> lift $ sendDeletionEmail n e k v l)
            (\p -> lift $ wrapHttp $ sendDeletionSms p k v l)
            target
            `onException` wrapClientE (Code.delete k Code.AccountDeletion)
          pure $! Just $! Code.codeTTL c

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
    Member (Input (Local ())) r,
    Member UserStore r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  VerifyDeleteUser ->
  ExceptT DeleteUserError (AppT r) ()
verifyDeleteUser d = do
  let key = verifyDeleteUserKey d
  let code = verifyDeleteUserCode d
  c <- lift . wrapClient $ Code.verify key Code.AccountDeletion code
  a <- maybe (throwE DeleteUserInvalidCode) pure (Code.codeAccount =<< c)
  account <- lift . wrapClient $ Data.lookupAccount (Id a)
  for_ account $ lift . liftSem . deleteAccount
  lift . wrapClient $ Code.delete key Code.AccountDeletion

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
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r,
    Member UserStore r
  ) =>
  UserId ->
  AppT r DeleteUserResult
ensureAccountDeleted uid = do
  mbAcc <- wrapClient $ lookupAccount uid
  case mbAcc of
    Nothing -> pure NoUser
    Just acc -> do
      probs <- wrapClient $ Data.lookupPropertyKeysAndValues uid

      let accIsDeleted = accountStatus acc == Deleted
      clients <- wrapClient $ Data.lookupClients uid

      localUid <- qualifyLocal uid
      conCount <- wrapClient $ countConnections localUid [(minBound @Relation) .. maxBound]
      cookies <- wrapClient $ Auth.listCookies uid []

      if notNull probs
        || not accIsDeleted
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
-- FUTUREWORK: this does not need the whole UserAccount structure, only the User.
deleteAccount ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member UserKeyStore r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member UserStore r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  UserAccount ->
  Sem r ()
deleteAccount (accountUser -> user) = do
  let uid = userId user
  Log.info $ field "user" (toByteString uid) . msg (val "Deleting account")
  do
    -- Free unique keys
    for_ (userEmail user) $ deleteKeyForUser uid . userEmailKey
    for_ (userPhone user) $ deleteKeyForUser uid . userPhoneKey

    embed $ Data.clearProperties uid

    deleteUser user

  Intra.rmUser uid (userAssets user)
  embed $ Data.lookupClients uid >>= mapM_ (Data.rmClient uid . clientId)
  luid <- embed $ qualifyLocal uid
  Intra.onUserEvent uid Nothing (UserDeleted (tUntagged luid))
  embed do
    -- Note: Connections can only be deleted afterwards, since
    --       they need to be notified.
    Data.deleteConnections uid
    Auth.revokeAllCookies uid

-------------------------------------------------------------------------------
-- Lookups

lookupActivationCode ::
  (MonadClient m) =>
  Either Email Phone ->
  m (Maybe ActivationPair)
lookupActivationCode emailOrPhone = do
  let uk = either userEmailKey userPhoneKey emailOrPhone
  k <- liftIO $ Data.mkActivationKey uk
  c <- fmap snd <$> Data.lookupActivationCode uk
  pure $ (k,) <$> c

lookupPasswordResetCode ::
  ( Member AuthenticationSubsystem r
  ) =>
  Either Email Phone ->
  (AppT r) (Maybe PasswordResetPair)
lookupPasswordResetCode emailOrPhone = do
  let uk = either userEmailKey userPhoneKey emailOrPhone
  liftSem $ internalLookupPasswordResetCode uk

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
        { Prom.metricName = "user.enqueue_multi_delete_total",
          Prom.metricHelp = "Number of users enqueued to be deleted"
        }

{-# NOINLINE enqueueMultiDeleteCallsCounter #-}
enqueueMultiDeleteCallsCounter :: Prom.Counter
enqueueMultiDeleteCallsCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user.enqueue_multi_delete_calls_total",
          Prom.metricHelp = "Number of users enqueued to be deleted"
        }

-- | Similar to lookupProfiles except it returns all results and all errors
-- allowing for partial success.
lookupProfilesV3 ::
  (Member UserSubsystem r) =>
  Local UserId ->
  -- | The users ('others') for which to obtain the profiles.
  [Qualified UserId] ->
  Sem r ([(Qualified UserId, FederationError)], [UserProfile])
lookupProfilesV3 self others = getUserProfilesWithErrors self others

getLegalHoldStatus ::
  (Member GalleyAPIAccess r) =>
  UserId ->
  AppT r (Maybe UserLegalHoldStatus)
getLegalHoldStatus uid = traverse (liftSem . getLegalHoldStatus' . accountUser) =<< wrapHttpClient (lookupAccount uid)

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

-- | Find user accounts for a given identity, both activated and those
-- currently pending activation.
lookupAccountsByIdentity :: (Member UserKeyStore r) => Either Email Phone -> Bool -> AppT r [UserAccount]
lookupAccountsByIdentity emailOrPhone includePendingInvitations = do
  let uk = either userEmailKey userPhoneKey emailOrPhone
  activeUid <- liftSem $ lookupKey uk
  uidFromKey <- (>>= fst) <$> wrapClient (Data.lookupActivationCode uk)
  result <- wrapClient $ Data.lookupAccounts (nub $ catMaybes [activeUid, uidFromKey])
  if includePendingInvitations
    then pure result
    else pure $ filter ((/= PendingInvitation) . accountStatus) result

isBlacklisted :: (Member BlacklistStore r) => Either Email Phone -> AppT r Bool
isBlacklisted emailOrPhone = do
  let uk = either userEmailKey userPhoneKey emailOrPhone
  liftSem $ BlacklistStore.exists uk

blacklistInsert :: (Member BlacklistStore r) => Either Email Phone -> AppT r ()
blacklistInsert emailOrPhone = do
  let uk = either userEmailKey userPhoneKey emailOrPhone
  liftSem $ BlacklistStore.insert uk

blacklistDelete :: (Member BlacklistStore r) => Either Email Phone -> AppT r ()
blacklistDelete emailOrPhone = do
  let uk = either userEmailKey userPhoneKey emailOrPhone
  liftSem $ BlacklistStore.delete uk

phonePrefixGet :: (Member BlacklistPhonePrefixStore r) => PhonePrefix -> (AppT r) [ExcludedPrefix]
phonePrefixGet = liftSem . BlacklistPhonePrefixStore.getAll

phonePrefixDelete :: (Member BlacklistPhonePrefixStore r) => PhonePrefix -> (AppT r) ()
phonePrefixDelete = liftSem . BlacklistPhonePrefixStore.delete

phonePrefixInsert :: (Member BlacklistPhonePrefixStore r) => ExcludedPrefix -> (AppT r) ()
phonePrefixInsert = liftSem . BlacklistPhonePrefixStore.insert
