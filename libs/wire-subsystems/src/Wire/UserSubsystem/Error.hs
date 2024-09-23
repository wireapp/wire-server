module Wire.UserSubsystem.Error where

import Imports
import Network.HTTP.Types (status404)
import Network.Wai.Utilities qualified as Wai
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
  | UserSubsystemCannotJoinMultipleTeams
  | UserSubsystemTooManyTeamMembers
  | UserSubsystemMissingAuth
  | UserSubsystemBadCredentials
  | UserSubsystemMissingIdentity
  | UserSubsystemInvalidActivationCodeWrongUser
  | UserSubsystemInvalidActivationCodeWrongCode
  | UserSubsystemInvalidInvitationCode
  | UserSubsystemInvitationNotFound
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
    UserSubsystemInsufficientTeamPermissions -> errorToWai @'E.InsufficientTeamPermissions
    UserSubsystemCannotJoinMultipleTeams -> errorToWai @E.CannotJoinMultipleTeams
    UserSubsystemTooManyTeamMembers -> errorToWai @E.TooManyTeamMembers
    UserSubsystemMissingAuth -> errorToWai @E.MissingAuth
    UserSubsystemBadCredentials -> errorToWai @E.BadCredentials
    UserSubsystemMissingIdentity -> errorToWai @E.MissingIdentity
    UserSubsystemInvalidActivationCodeWrongUser -> errorToWai @E.InvalidActivationCodeWrongUser
    UserSubsystemInvalidActivationCodeWrongCode -> errorToWai @E.InvalidActivationCodeWrongCode
    UserSubsystemInvalidInvitationCode -> errorToWai @E.InvalidInvitationCode
    UserSubsystemInvitationNotFound -> Wai.mkError status404 "not-found" "Something went wrong, while looking up the invitation"

instance Exception UserSubsystemError
