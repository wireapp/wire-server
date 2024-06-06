module Wire.UserSubsystem.Error where

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
import Wire.UserKeyStore

-- | All errors that are thrown by the user subsystem are subsumed under this sum type.
data UserSubsystemError
  = -- | user is managed by scim or e2ei is enabled
    --   FUTUREWORK(mangoiv): the name should probably resemble that
    UserSubsystemDisplayNameManagedByScim
  | UserSubsystemHandleManagedByScim
  | UserSubsystemLocaleManagedByScim
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
    UserSubsystemLocaleManagedByScim -> dynError @(MapError E.LocaleManagedByScim)
    UserSubsystemNoIdentity -> dynError @(MapError E.NoIdentity)
    UserSubsystemHandleExists -> dynError @(MapError E.HandleExists)
    UserSubsystemInvalidHandle -> dynError @(MapError E.InvalidHandle)
    UserSubsystemHandleManagedByScim -> dynError @(MapError E.HandleManagedByScim)

instance Exception UserSubsystemError
