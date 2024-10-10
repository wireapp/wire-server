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
import SAML2.WebSSO.Types (UserRef)
import Wire.API.Federation.Error
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti (TeamStatus)
import Wire.API.Team.Feature
import Wire.API.Team.Member (IsPerm (..), TeamMember)
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Search
import Wire.Arbitrary
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.InvitationStore
import Wire.UserKeyStore
import Wire.UserSearch.Types
import Wire.UserSubsystem.Error (UserSubsystemError (..))

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
  -- | Initiate change of email address
  UpdateUserEmailInit :: UserId -> EmailAddress -> UserSubsystem m ChangeEmailResponse
  -- | Complete the email address update flow
  UpdateUserEmailComplete :: Activate -> UserSubsystem m ActivationFullResponse
  -- | Update SAML IdP EntityId (Issuer) and User NameId
  UpdateUserSamlUserRef :: UserRef -> UserSubsystem m ()
  -- | Update SCIM externalId
  UpdateUserScimExternalId :: Text -> UserSubsystem m ()
  -- | parse and lookup a handle, return what the operation has found
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

getLocalUser :: (Member UserSubsystem r) => Local UserId -> Sem r (Maybe User)
getLocalUser = (selfUser <$$>) . getSelfProfile

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

getLocalUserAccountByUserKey :: (Member UserSubsystem r) => Local EmailKey -> Sem r (Maybe User)
getLocalUserAccountByUserKey q@(tUnqualified -> ek) =
  listToMaybe <$> getAccountsByEmailNoFilter (qualifyAs q [emailKeyOrig ek])

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
