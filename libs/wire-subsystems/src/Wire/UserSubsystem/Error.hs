module Wire.UserSubsystem.Error where

import Data.Text.Lazy qualified as LT
import Imports
import Network.HTTP.Types (status400, status403, status404)
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
  | UserSubsystemEmailManagedByScim
  | UserSubsystemNoIdentity
  | UserSubsystemLastIdentity
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
  | UserSubsystemChangeBlocklistedEmail
  | UserSubsystemEmailExists
  | UserSubsystemRegistrationForbiddenForDomain
  | UserSubsystemInvalidDomain String
  deriving (Eq, Show)

userSubsystemErrorToHttpError :: UserSubsystemError -> HttpError
userSubsystemErrorToHttpError =
  StdError . \case
    UserSubsystemProfileNotFound -> errorToWai @E.UserNotFound
    UserSubsystemDisplayNameManagedByScim -> errorToWai @E.NameManagedByScim
    UserSubsystemLocaleManagedByScim -> errorToWai @E.LocaleManagedByScim
    UserSubsystemEmailManagedByScim -> errorToWai @E.EmailManagedByScim
    UserSubsystemNoIdentity -> errorToWai @E.NoIdentity
    UserSubsystemLastIdentity -> errorToWai @E.LastIdentity
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
    UserSubsystemChangeBlocklistedEmail -> errorToWai @E.BlacklistedEmail
    UserSubsystemEmailExists -> errorToWai @'E.UserKeyExists
    UserSubsystemRegistrationForbiddenForDomain -> Wai.mkError status403 "registration-forbidden-for-domain" "Domain is configured for enterprise login."
    UserSubsystemInvalidDomain msg -> Wai.mkError status400 "invalid-domain" (LT.pack msg)

instance Exception UserSubsystemError
