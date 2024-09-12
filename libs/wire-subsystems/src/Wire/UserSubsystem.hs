{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.UserSubsystem
  ( module Wire.UserSubsystem,
    module Data.HavePendingInvitations,
  )
where

import Data.Default
import Data.Handle (Handle)
import Data.HavePendingInvitations
import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Federation.Error
import Wire.API.User
import Wire.Arbitrary
import Wire.UserKeyStore (EmailKey, emailKeyOrig)

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
  GetExtendedAccountsBy :: Local GetBy -> UserSubsystem m [ExtendedUserAccount]
  -- | Get user accounts matching the `[EmailAddress]` argument (accounts with missing
  -- identity and accounts with status /= active included).
  GetExtendedAccountsByEmailNoFilter :: Local [EmailAddress] -> UserSubsystem m [ExtendedUserAccount]
  -- | Get user account by local user id (accounts with missing identity and accounts with
  -- status /= active included).
  GetAccountNoFilter :: Local UserId -> UserSubsystem m (Maybe UserAccount)
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

-- | the return type of 'CheckHandle'
data CheckHandleResp
  = CheckHandleFound
  | CheckHandleNotFound
  deriving stock (Eq, Ord, Show)

makeSem ''UserSubsystem

-- | given a lookup criteria record ('GetBy'), return the union of the user accounts fulfilling that criteria
getAccountsBy :: (Member UserSubsystem r) => Local GetBy -> Sem r [UserAccount]
getAccountsBy getby = (.account) <$$> getExtendedAccountsBy getby

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
  Sem r (Maybe UserAccount)
getLocalAccountBy includePendingInvitations uid =
  listToMaybe
    <$> getAccountsBy
      ( qualifyAs uid $
          def
            { getByUserId = [tUnqualified uid],
              includePendingInvitations
            }
      )

getLocalUserAccountByUserKey :: (Member UserSubsystem r) => Local EmailKey -> Sem r (Maybe UserAccount)
getLocalUserAccountByUserKey q@(tUnqualified -> ek) =
  listToMaybe . fmap (.account) <$> getExtendedAccountsByEmailNoFilter (qualifyAs q [emailKeyOrig ek])
