{-# LANGUAGE TemplateHaskell #-}

module Wire.UserSubsystem where

import Data.Id
import Data.Qualified
import Imports
import Network.Wai.Utilities qualified as Wai
import Polysemy
import Wire.API.Error
-- FUTUREWORK(mangoiv): this should probably be renamed such that it doesn't
-- associate with the name "brig" anymore
import Wire.API.Error.Brig (BrigError (..))
import Wire.API.Federation.Error
import Wire.API.User
import Wire.UserStore

-- | All errors that are thrown by the user subsystem are subsumed under this sum type.
data UserSubsystemError
  = -- | user is managed by scim or e2ei is enabled
    --   FUTUREWORK(mangoiv): the name should probably resemble that
    UserSubsystemDisplayNameManagedByScim
  | UserSubsystemProfileNotFound
  deriving (Eq, Show)

userSubsystemErrorToWai :: UserSubsystemError -> Wai.Error
userSubsystemErrorToWai =
  dynErrorToWai . \case
    UserSubsystemProfileNotFound -> dynError @(MapError UserNotFound)
    UserSubsystemDisplayNameManagedByScim -> dynError @(MapError NameManagedByScim)

instance Exception UserSubsystemError

data UserSubsystem m a where
  -- | First arg is for authorization only.
  GetUserProfiles :: Local UserId -> [Qualified UserId] -> UserSubsystem m [UserProfile]
  -- | Sometimes we don't have any identity of a requesting user, and local profiles are public.
  GetLocalUserProfiles :: Local [UserId] -> UserSubsystem m [UserProfile]
  -- | These give us partial success and hide concurrency in the interpreter.
  -- FUTUREWORK: it would be better to return errors as `Map Domain FederationError`, but would clients like that?
  GetUserProfilesWithErrors :: Local UserId -> [Qualified UserId] -> UserSubsystem m ([(Qualified UserId, FederationError)], [UserProfile])
  UpdateUserProfile :: Local UserId -> Maybe ConnId -> UserProfileUpdate -> UserSubsystem m ()

makeSem ''UserSubsystem

getUserProfile :: Member UserSubsystem r => Local UserId -> Qualified UserId -> Sem r (Maybe UserProfile)
getUserProfile luid targetUser =
  listToMaybe <$> getUserProfiles luid [targetUser]

getLocalUserProfile :: Member UserSubsystem r => Local UserId -> Sem r (Maybe UserProfile)
getLocalUserProfile targetUser =
  listToMaybe <$> getLocalUserProfiles ((: []) <$> targetUser)
