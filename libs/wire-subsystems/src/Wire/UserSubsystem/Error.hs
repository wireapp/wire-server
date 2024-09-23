module Wire.UserSubsystem.Error where

import Imports
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.Error

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
  | UserSubsystemInsufficientTeamPermissions
  | UserSubsystemMLSServicesNotAllowed
  deriving (Eq, Show)

userSubsystemErrorToHttpError :: UserSubsystemError -> HttpError
userSubsystemErrorToHttpError =
  StdError . \case
    UserSubsystemProfileNotFound -> errorToWai @E.UserNotFound
    UserSubsystemDisplayNameManagedByScim -> errorToWai @E.NameManagedByScim
    UserSubsystemLocaleManagedByScim -> errorToWai @E.LocaleManagedByScim
    UserSubsystemNoIdentity -> errorToWai @E.NoIdentity
    UserSubsystemHandleExists -> errorToWai @E.HandleExists
    UserSubsystemInvalidHandle -> errorToWai @E.InvalidHandle
    UserSubsystemHandleManagedByScim -> errorToWai @E.HandleManagedByScim
    UserSubsystemInsufficientTeamPermissions -> errorToWai @E.InsufficientTeamPermissions
    UserSubsystemMLSServicesNotAllowed -> errorToWai @E.MLSServicesNotAllowed

instance Exception UserSubsystemError
