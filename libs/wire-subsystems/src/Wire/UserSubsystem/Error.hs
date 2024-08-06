module Wire.UserSubsystem.Error where

import Imports
import Network.HTTP.Types (status404)
import Network.Wai.Utilities qualified as Wai
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.User.Identity
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
  | UserSubsystemMissingIdentity
  | UserSubsystemInvalidActivationCodeWrongUser
  | UserSubsystemInvalidActivationCodeWrongCode
  | UserSubsystemInvalidInvitationCode
  | UserSubsystemInvitationNotFound
  | UserSubsystemUserNotAllowedToJoinTeam Wai.Error
  | UserSubsystemMLSServicesNotAllowed
  | UserSubsystemChangeEmailError ChangeEmailError
  deriving (Eq, Show)

data ChangeEmailError
  = InvalidNewEmail !Email !String
  | EmailExists !Email
  | ChangeBlacklistedEmail !Email
  | EmailManagedByScim
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
    UserSubsystemCannotJoinMultipleTeams -> errorToWai @E.CannotJoinMultipleTeams
    UserSubsystemTooManyTeamMembers -> errorToWai @E.TooManyTeamMembers
    UserSubsystemMissingIdentity -> errorToWai @E.MissingIdentity
    UserSubsystemInvalidActivationCodeWrongUser -> errorToWai @E.InvalidActivationCodeWrongUser
    UserSubsystemInvalidActivationCodeWrongCode -> errorToWai @E.InvalidActivationCodeWrongCode
    UserSubsystemInvalidInvitationCode -> errorToWai @E.InvalidInvitationCode
    UserSubsystemInvitationNotFound -> Wai.mkError status404 "not-found" "Something went wrong, while looking up the invitation"
    UserSubsystemUserNotAllowedToJoinTeam e -> e
    UserSubsystemMLSServicesNotAllowed -> errorToWai @E.MLSServicesNotAllowed
    UserSubsystemChangeEmailError _ -> _ -- check how this is handled in brig! is it also an api error? if not: is it ok to throw it here anyway?

instance Exception UserSubsystemError
