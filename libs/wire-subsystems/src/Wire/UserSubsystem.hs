{-# LANGUAGE TemplateHaskell #-}

module Wire.UserSubsystem where

import Data.Default
import Data.Handle (Handle)
import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Federation.Error
import Wire.API.User
import Wire.Arbitrary
import Wire.UserKeyStore

-- | Who is performing this update operation?  (Single source of truth: users managed by SCIM
-- can't be updated by clients and vice versa.)
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
-- belong to different abstractions / levels (UserSubsystem vs. UserStore), and they may
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

data UserSubsystem m a where
  -- | First arg is for authorization only.
  GetUserProfiles :: Local UserId -> [Qualified UserId] -> UserSubsystem m [UserProfile]
  -- | Sometimes we don't have any identity of a requesting user, and local profiles are public.
  GetLocalUserProfiles :: Local [UserId] -> UserSubsystem m [UserProfile]
  -- | Self profile contains things not present in Profile.
  GetSelfProfile :: Local UserId -> UserSubsystem m (Maybe SelfProfile)
  -- | These give us partial success and hide concurrency in the interpreter.
  -- FUTUREWORK: it would be better to return errors as `Map Domain FederationError`, but would clients like that?
  GetUserProfilesWithErrors :: Local UserId -> [Qualified UserId] -> UserSubsystem m ([(Qualified UserId, FederationError)], [UserProfile])
  -- | Simple updates (as opposed to, eg., handle, where we need to manage locks).  Empty fields are ignored (not deleted).
  UpdateUserProfile :: Local UserId -> Maybe ConnId -> UpdateOriginType -> UserProfileUpdate -> UserSubsystem m ()
  -- | parse and lookup a handle, return what the operation has found
  CheckHandle :: Text {- use Handle here? -} -> UserSubsystem m CheckHandleResp
  -- | checks a number of 'Handle's for availability and returns at most 'Word' amount of them
  CheckHandles :: [Handle] -> Word -> UserSubsystem m [Handle]
  -- | parses a handle, this may fail so it's effectful
  UpdateHandle :: Local UserId -> Maybe ConnId -> UpdateOriginType -> Text {- use Handle here? -} -> UserSubsystem m ()
  GetLocalUserAccountByUserKey :: Local EmailKey -> UserSubsystem m (Maybe UserAccount)
  -- | returns the user's locale or the default locale if the users exists
  LookupLocaleWithDefault :: Local UserId -> UserSubsystem m (Maybe Locale)

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

updateSupportedProtocols :: (Member UserSubsystem r) => Local UserId -> UpdateOriginType -> Set BaseProtocolTag -> Sem r ()
updateSupportedProtocols uid mb prots = updateUserProfile uid Nothing mb (def {supportedProtocols = Just prots})
