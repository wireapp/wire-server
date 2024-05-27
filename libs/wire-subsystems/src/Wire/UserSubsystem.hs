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
  | -- TODO: UserSubsystemLocaleManagedByScim
    UserSubsystemHandleManagedByScim
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
    -- TODO: UserSubsystemLocaleManagedByScim -> dynError @(MapError E.NameManagedByScim)
    UserSubsystemNoIdentity -> dynError @(MapError E.NoIdentity)
    UserSubsystemHandleExists -> dynError @(MapError E.HandleExists)
    UserSubsystemInvalidHandle -> dynError @(MapError E.InvalidHandle)
    UserSubsystemHandleManagedByScim -> dynError @(MapError E.HandleManagedByScim)

instance Exception UserSubsystemError

-- TODO: rename to data UpdateOriginScim = UpdateOriginScim | UpdateOriginWireClient
data AllowSCIMUpdates
  = AllowSCIMUpdates
  | ForbidSCIMUpdates
  deriving (Show, Eq, Ord, Generic)
  deriving (Arbitrary) via GenericUniform AllowSCIMUpdates

-- TODO: is UserUpdate redundant now?

-- | Simple updates (as opposed to, eg., handle, where we need to manage locks).
data UserProfileUpdate = MkUserProfileUpdate
  { name :: Maybe Name,
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
  -- | These give us partial success and hide concurrency in the interpreter.
  -- FUTUREWORK: it would be better to return errors as `Map Domain FederationError`, but would clients like that?
  GetUserProfilesWithErrors :: Local UserId -> [Qualified UserId] -> UserSubsystem m ([(Qualified UserId, FederationError)], [UserProfile])
  -- | Simple updates (as opposed to, eg., handle, where we need to manage locks).  Empty fields are ignored (not deleted).
  UpdateUserProfile :: Local UserId -> Maybe ConnId -> AllowSCIMUpdates -> UserProfileUpdate -> UserSubsystem m ()
  -- | parse and lookup a handle, return what the operation has found
  CheckHandle :: Text -> UserSubsystem m CheckHandleResp
  -- | checks a number of 'Handle's for availability and returns at most 'Word' amount of them
  CheckHandles :: [Handle] -> Word -> UserSubsystem m [Handle]
  -- | parses a handle, this may fail so it's effectful
  UpdateHandle :: AllowSCIMUpdates -> Text -> UserSubsystem m ()

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

updateSupportedProtocols :: Member UserSubsystem r => Local UserId -> AllowSCIMUpdates -> Set BaseProtocolTag -> Sem r ()
updateSupportedProtocols uid mb prots = updateUserProfile uid Nothing mb (def {supportedProtocols = Just prots})
