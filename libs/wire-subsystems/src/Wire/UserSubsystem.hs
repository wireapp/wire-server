{-# LANGUAGE TemplateHaskell #-}

module Wire.UserSubsystem where

-- FUTUREWORK(mangoiv): this should probably be renamed such that it doesn't
-- associate with the name "brig" anymore

import Data.Default
import Data.Handle (Handle)
import Data.Id
import Data.Qualified
import Imports
import Network.Wai.Utilities qualified as Wai
import Polysemy
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Federation.Error
import Wire.API.User
import Wire.Arbitrary

-- | All errors that are thrown by the user subsystem are subsumed under this sum type.
data UserSubsystemError
  = -- | user is managed by scim or e2ei is enabled
    --   FUTUREWORK(mangoiv): the name should probably resemble that
    UserSubsystemDisplayNameManagedByScim
  | UserSubsystemHandleManagedByScim
  | UserSubsystemNoIdentity
  | UserSubsystemHandleExists
  | UserSubsystemInvalidHandle
  | UserSubsystemProfileNotFound
  deriving (Eq, Show)

userSubsystemErrorToWai :: UserSubsystemError -> Wai.Error
userSubsystemErrorToWai =
  dynErrorToWai . \case
    UserSubsystemProfileNotFound -> dynError @(MapError E.UserNotFound)
    UserSubsystemDisplayNameManagedByScim -> dynError @(MapError E.NameManagedByScim)
    UserSubsystemNoIdentity -> dynError @(MapError E.NoIdentity)
    UserSubsystemHandleExists -> dynError @(MapError E.HandleExists)
    UserSubsystemInvalidHandle -> dynError @(MapError E.InvalidHandle)
    UserSubsystemHandleManagedByScim -> dynError @(MapError E.HandleManagedByScim)

instance Exception UserSubsystemError

data AllowSCIMUpdates
  = AllowSCIMUpdates
  | ForbidSCIMUpdates
  deriving (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericUniform AllowSCIMUpdates

-- | Wrapper around an updated field which can potentially be managed by SCIM.
data ScimUpdate a = MkScimUpdate
  { -- | whether changes to SCIM-managed users should be allowed
    allowScim :: AllowSCIMUpdates,
    value :: a
  }
  deriving stock (Eq, Ord, Show)
  deriving (Functor, Foldable, Traversable)

instance Arbitrary a => Arbitrary (ScimUpdate a) where
  arbitrary = MkScimUpdate <$> arbitrary <*> arbitrary

forbidScimUpdate :: a -> ScimUpdate a
forbidScimUpdate = MkScimUpdate ForbidSCIMUpdates

allowScimUpdate :: a -> ScimUpdate a
allowScimUpdate = MkScimUpdate AllowSCIMUpdates

-- this is similar to `UserUpdate` in `Wire.API.User`, but supports updates to
-- all profile fields rather than just four.
data UserProfileUpdate = MkUserProfileUpdate
  { name :: Maybe (ScimUpdate Name),
    pict :: Maybe Pict,
    assets :: Maybe [Asset],
    accentId :: Maybe ColourId,
    locale :: Maybe Locale,
    handle :: Maybe (ScimUpdate Handle)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via GenericUniform UserProfileUpdate

instance Default UserProfileUpdate where
  def =
    MkUserProfileUpdate
      { name = Nothing,
        pict = Nothing,
        assets = Nothing,
        accentId = Nothing,
        locale = Nothing,
        handle = Nothing
      }

data UserSubsystem m a where
  -- | First arg is for authorization only.
  GetUserProfiles :: Local UserId -> [Qualified UserId] -> UserSubsystem m [UserProfile]
  -- | Sometimes we don't have any identity of a requesting user, and local profiles are public.
  GetLocalUserProfiles :: Local [UserId] -> UserSubsystem m [UserProfile]
  -- | These give us partial success and hide concurrency in the interpreter.
  -- FUTUREWORK: it would be better to return errors as `Map Domain FederationError`, but would clients like that?
  GetUserProfilesWithErrors :: Local UserId -> [Qualified UserId] -> UserSubsystem m ([(Qualified UserId, FederationError)], [UserProfile])
  UpdateUserProfile :: Local UserId -> Maybe ConnId -> UserProfileUpdate -> UserSubsystem m ()
  -- | parse and lookup a handle, return what the operation has found
  CheckHandle :: Text -> UserSubsystem m CheckHandleResp
  -- | checks a number of 'Handle's for availability and returns at most 'Word' amount of them
  CheckHandles :: [Handle] -> Word -> UserSubsystem m [Handle]
  -- | parses a handle, this may fail so it's effectful
  ParseHandle :: Text -> UserSubsystem m Handle

-- | the return type of 'CheckHandle'
data CheckHandleResp
  = CheckHandleFound
  | CheckHandleNotFound
  deriving stock (Eq, Ord, Show)

makeSem ''UserSubsystem

getUserProfile :: Member UserSubsystem r => Local UserId -> Qualified UserId -> Sem r (Maybe UserProfile)
getUserProfile luid targetUser =
  listToMaybe <$> getUserProfiles luid [targetUser]

getLocalUserProfile :: Member UserSubsystem r => Local UserId -> Sem r (Maybe UserProfile)
getLocalUserProfile targetUser =
  listToMaybe <$> getLocalUserProfiles ((: []) <$> targetUser)
