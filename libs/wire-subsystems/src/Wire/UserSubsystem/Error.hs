module Wire.UserSubsystem.Error where

import Data.Text.Lazy qualified as LT
import Imports
import Network.HTTP.Types (status400, status403, status404)
import Network.Wai.Utilities qualified as Wai
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.User
import Wire.Arbitrary
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
  | UserSubsystemGuardFailed GuardFailure
  | UserSubsystemRegisterError RegisterError
  deriving (Eq, Show)

data GuardFailure
  = DomRedirSetToSSO
  | DomRedirSetToBackend
  | DomRedirSetToNoRegistration
  | TeamInviteSetToNotAllowed
  | TeamInviteRestrictedToOtherTeam
  | InvalidDomain String
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform GuardFailure)

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
    UserSubsystemGuardFailed err ->
      let e403 msg = Wai.mkError status403 "condition-failed" msg
          e400 msg = Wai.mkError status400 "invalid-domain" (LT.pack msg)
       in case err of
            DomRedirSetToSSO -> e403 "`domain_redirect` is set to `sso:{code}`"
            DomRedirSetToBackend -> e403 "`domain_redirect` is set to `backend`"
            DomRedirSetToNoRegistration -> e403 "`domain_redirect` is set to `no-registration`"
            TeamInviteSetToNotAllowed -> e403 "`teamInvite` is set to `not-allowed`"
            TeamInviteRestrictedToOtherTeam -> e403 "`teamInvite` is restricted to another team."
            InvalidDomain parseErr -> e400 parseErr
    UserSubsystemRegisterError _ -> todo

instance Exception UserSubsystemError
