{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.UserSubsystem
  ( module Wire.UserSubsystem,
    module Data.HavePendingInvitations,
  )
where

import Data.Default
import Data.Domain
import Data.Handle (Handle)
import Data.HavePendingInvitations
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Range
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Federation.Error
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti (TeamStatus)
import Wire.API.Team.Export (TeamExportUser)
import Wire.API.Team.Feature
import Wire.API.Team.Member (IsPerm (..), TeamMember)
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Search
import Wire.ActivationCodeStore
import Wire.Arbitrary
import Wire.BlockListStore
import Wire.BlockListStore qualified as BlockListStore
import Wire.EmailSubsystem
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.InvitationStore
import Wire.UserKeyStore
import Wire.UserSearch.Types
import Wire.UserStore
import Wire.UserSubsystem.Error (UserSubsystemError (..))
import Wire.UserSubsystem.UserSubsystemConfig

-- | Who is performing this update operation / who is allowed to?  (Single source of truth:
-- users managed by SCIM can't be updated by clients and vice versa.)
data UpdateOriginType
  = -- | Call originates from the SCIM api in spar.
    UpdateOriginScim
  | -- | Call originates from wire client (mobile, web, or team-management).
    UpdateOriginWireClient
  deriving (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericUniform UpdateOriginType

-- | Simple updates (as opposed to, eg., handle, where we need to manage locks).
--
-- This is isomorphic to 'StoredUserUpdate', but we keep the two types separate because they
-- belong to different abstraction levels (UserSubsystem vs. UserStore), and they may
-- change independently in the future ('UserStoreUpdate' may grow more fields for other
-- operations).
data UserProfileUpdate = MkUserProfileUpdate
  { name :: Maybe Name,
    textStatus :: Maybe TextStatus,
    pict :: Maybe Pict, -- DEPRECATED
    assets :: Maybe [Asset],
    accentId :: Maybe ColourId,
    locale :: Maybe Locale,
    supportedProtocols :: Maybe (Set BaseProtocolTag)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UserProfileUpdate

instance Default UserProfileUpdate where
  def =
    MkUserProfileUpdate
      { name = Nothing,
        textStatus = Nothing,
        pict = Nothing, -- DEPRECATED
        assets = Nothing,
        accentId = Nothing,
        locale = Nothing,
        supportedProtocols = Nothing
      }

-- | Parameters for `getExternalAccountsBy` operation below.
data GetBy = MkGetBy
  { -- | whether or not to include pending invitations when getting users by ids.
    includePendingInvitations :: HavePendingInvitations,
    -- | get accounts by 'UserId'.
    getByUserId :: [UserId],
    -- | get accounts by their 'Handle'
    getByHandle :: [Handle]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform GetBy

instance Default GetBy where
  def = MkGetBy NoPendingInvitations [] []

-- | Outcome of email change invariant checks.
data ChangeEmailResult
  = -- | The request was successful, user needs to verify the new email address
    ChangeEmailNeedsActivation !(User, Activation, EmailAddress)
  | -- | The user asked to change the email address to the one already owned
    ChangeEmailIdempotent
  deriving (Show)

data UserSubsystem m a where
  -- | First arg is for authorization only.
  GetUserProfiles :: Local UserId -> [Qualified UserId] -> UserSubsystem m [UserProfile]
  -- | These give us partial success and hide concurrency in the interpreter.
  -- (Nit-pick: a better return type for this might be `([Qualified ([UserId],
  -- FederationError)], [UserProfile])`, and then we'd probably need a function of type
  -- `([Qualified ([UserId], FederationError)], [UserProfile]) -> ([(Qualified UserId,
  -- FederationError)], [UserProfile])` to maintain API compatibility.)
  GetUserProfilesWithErrors :: Local UserId -> [Qualified UserId] -> UserSubsystem m ([(Qualified UserId, FederationError)], [UserProfile])
  -- | Sometimes we don't have any identity of a requesting user, and local profiles are public.
  GetLocalUserProfiles :: Local [UserId] -> UserSubsystem m [UserProfile]
  -- | Get the union of all user accounts matching the `GetBy` argument *and* having a non-empty UserIdentity.
  GetAccountsBy :: Local GetBy -> UserSubsystem m [User]
  -- | Get user accounts matching the `[EmailAddress]` argument (accounts with missing
  -- identity and accounts with status /= active included).
  GetAccountsByEmailNoFilter :: Local [EmailAddress] -> UserSubsystem m [User]
  -- | Get user account by local user id (accounts with missing identity and accounts with
  -- status /= active included).
  GetAccountNoFilter :: Local UserId -> UserSubsystem m (Maybe User)
  -- | Get `SelfProfile` (it contains things not present in `UserProfile`).
  GetSelfProfile :: Local UserId -> UserSubsystem m (Maybe SelfProfile)
  -- | Simple updates (as opposed to, eg., handle, where we need to manage locks).  Empty fields are ignored (not deleted).
  UpdateUserProfile :: Local UserId -> Maybe ConnId -> UpdateOriginType -> UserProfileUpdate -> UserSubsystem m ()
  -- | Parse and lookup a handle.
  CheckHandle :: Text {- use Handle here? -} -> UserSubsystem m CheckHandleResp
  -- | Check a number of 'Handle's for availability and returns at most 'Word' amount of them
  CheckHandles :: [Handle] -> Word -> UserSubsystem m [Handle]
  -- | Parse and update a handle. Parsing may fail so this is effectful.
  UpdateHandle :: Local UserId -> Maybe ConnId -> UpdateOriginType -> Text {- use Handle here? -} -> UserSubsystem m ()
  -- | Return the user's locale (or the default locale if the users exists and has none).
  LookupLocaleWithDefault :: Local UserId -> UserSubsystem m (Maybe Locale)
  -- | Check if an email is blocked.
  IsBlocked :: EmailAddress -> UserSubsystem m Bool
  -- | Remove an email from the block list.
  BlockListDelete :: EmailAddress -> UserSubsystem m ()
  -- | Add an email to the block list.
  BlockListInsert :: EmailAddress -> UserSubsystem m ()
  UpdateTeamSearchVisibilityInbound :: TeamStatus SearchVisibilityInboundConfig -> UserSubsystem m ()
  SearchUsers ::
    Local UserId ->
    Text ->
    Maybe Domain ->
    Maybe (Range 1 500 Int32) ->
    UserSubsystem m (SearchResult Contact)
  BrowseTeam ::
    UserId ->
    BrowseTeamFilters ->
    Maybe (Range 1 500 Int) ->
    Maybe PagingState ->
    UserSubsystem m (SearchResult TeamContact)
  -- | (...  or does `AcceptTeamInvitation` belong into `TeamInvitationSubsystems`?)
  AcceptTeamInvitation :: Local UserId -> PlainTextPassword6 -> InvitationCode -> UserSubsystem m ()
  -- | The following "internal" functions exists to support migration in this susbystem, after the
  -- migration this would just be an internal detail of the subsystem
  InternalUpdateSearchIndex :: UserId -> UserSubsystem m ()
  InternalFindTeamInvitation :: Maybe EmailKey -> InvitationCode -> UserSubsystem m StoredInvitation
  GetUserExportData :: UserId -> UserSubsystem m (Maybe TeamExportUser)
  RemoveEmailEither :: Local UserId -> UserSubsystem m (Either UserSubsystemError ())

-- | the return type of 'CheckHandle'
data CheckHandleResp
  = CheckHandleFound
  | CheckHandleNotFound
  deriving stock (Eq, Ord, Show)

makeSem ''UserSubsystem

getUserProfile :: (Member UserSubsystem r) => Local UserId -> Qualified UserId -> Sem r (Maybe UserProfile)
getUserProfile luid targetUser =
  listToMaybe <$> getUserProfiles luid [targetUser]

getLocalUserProfile :: (Member UserSubsystem r) => Local UserId -> Sem r (Maybe UserProfile)
getLocalUserProfile targetUser =
  listToMaybe <$> getLocalUserProfiles ((: []) <$> targetUser)

getLocalAccountBy ::
  (Member UserSubsystem r) =>
  HavePendingInvitations ->
  Local UserId ->
  Sem r (Maybe User)
getLocalAccountBy includePendingInvitations uid =
  listToMaybe
    <$> getAccountsBy
      ( qualifyAs uid $
          def
            { getByUserId = [tUnqualified uid],
              includePendingInvitations
            }
      )

getUserEmail :: (Member UserSubsystem r) => Local UserId -> Sem r (Maybe EmailAddress)
getUserEmail lusr =
  (>>= userEmail) <$> getLocalAccountBy WithPendingInvitations lusr

getLocalUserAccountByUserKey :: (Member UserSubsystem r) => Local EmailKey -> Sem r (Maybe User)
getLocalUserAccountByUserKey q@(tUnqualified -> ek) =
  listToMaybe <$> getAccountsByEmailNoFilter (qualifyAs q [emailKeyOrig ek])

-- | Call 'createEmailChangeToken' and process result: if email changes to
-- itself, succeed, if not, send validation email.
requestEmailChange ::
  ( Member BlockListStore r,
    Member UserKeyStore r,
    Member EmailSubsystem r,
    Member UserSubsystem r,
    Member UserStore r,
    Member (Error UserSubsystemError) r,
    Member ActivationCodeStore r,
    Member (Input UserSubsystemConfig) r
  ) =>
  Local UserId ->
  EmailAddress ->
  UpdateOriginType ->
  Sem r ChangeEmailResponse
requestEmailChange lusr email allowScim = do
  let u = tUnqualified lusr
  createEmailChangeToken lusr email allowScim >>= \case
    ChangeEmailIdempotent ->
      pure ChangeEmailResponseIdempotent
    ChangeEmailNeedsActivation (usr, adata, en) -> do
      sendOutEmail usr adata en
      updateEmailUnvalidated u email
      internalUpdateSearchIndex u
      pure ChangeEmailResponseNeedsActivation
  where
    sendOutEmail usr adata en = do
      (maybe sendActivationMail (const sendEmailAddressUpdateMail) usr.userIdentity)
        en
        (userDisplayName usr)
        (activationKey adata)
        (activationCode adata)
        (Just (userLocale usr))

-- | Prepare changing the email (checking a number of invariants).
createEmailChangeToken ::
  ( Member BlockListStore r,
    Member UserKeyStore r,
    Member (Error UserSubsystemError) r,
    Member UserSubsystem r,
    Member ActivationCodeStore r,
    Member (Input UserSubsystemConfig) r
  ) =>
  Local UserId ->
  EmailAddress ->
  UpdateOriginType ->
  Sem r ChangeEmailResult
createEmailChangeToken lusr email updateOrigin = do
  let ek = mkEmailKey email
      u = tUnqualified lusr
  blocklisted <- BlockListStore.exists ek
  when blocklisted $ throw UserSubsystemChangeBlocklistedEmail
  available <- keyAvailable ek (Just u)
  unless available $ throw UserSubsystemEmailExists
  usr <-
    getLocalAccountBy WithPendingInvitations lusr
      >>= note UserSubsystemProfileNotFound
  case emailIdentity =<< userIdentity usr of
    -- The user already has an email address and the new one is exactly the same
    Just current | current == email -> pure ChangeEmailIdempotent
    _ -> do
      unless (userManagedBy usr /= ManagedByScim || updateOrigin == UpdateOriginScim) $
        throw UserSubsystemEmailManagedByScim
      actTimeout <- inputs (.activationCodeTimeout)
      act <- newActivationCode ek actTimeout (Just u)
      pure $ ChangeEmailNeedsActivation (usr, act, email)

------------------------------------------
-- FUTUREWORK: Pending functions for a team subsystem
------------------------------------------

ensurePermissions ::
  ( IsPerm perm,
    Member GalleyAPIAccess r,
    Member (Error UserSubsystemError) r
  ) =>
  UserId ->
  TeamId ->
  [perm] ->
  Sem r ()
ensurePermissions u t perms = do
  m <- GalleyAPIAccess.getTeamMember u t
  unless (check m) $
    throw UserSubsystemInsufficientTeamPermissions
  where
    check :: Maybe TeamMember -> Bool
    check (Just m) = all (hasPermission m) perms
    check Nothing = False
