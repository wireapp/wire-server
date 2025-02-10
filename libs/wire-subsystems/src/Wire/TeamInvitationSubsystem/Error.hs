module Wire.TeamInvitationSubsystem.Error where

import Imports
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error qualified as Wai
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.Error

data TeamInvitationSubsystemError
  = TeamInvitationNoEmail
  | TeamInvitationInsufficientTeamPermissions
  | TooManyTeamInvitations
  | TeamInvitationBlacklistedEmail
  | TeamInvitationEmailTaken
  | TeamInvitationInvalidEmail
  | TeamInvitationNotAllowedForEmail
  deriving (Eq, Show)

instance Exception TeamInvitationSubsystemError

teamInvitationErrorToHttpError :: TeamInvitationSubsystemError -> HttpError
teamInvitationErrorToHttpError =
  StdError . \case
    TeamInvitationNoEmail -> errorToWai @E.NoEmail
    TeamInvitationInsufficientTeamPermissions -> errorToWai @E.InsufficientTeamPermissions
    TooManyTeamInvitations -> errorToWai @E.TooManyTeamInvitations
    TeamInvitationBlacklistedEmail -> errorToWai @E.BlacklistedEmail
    TeamInvitationEmailTaken -> errorToWai @E.EmailExists
    TeamInvitationInvalidEmail -> errorToWai @E.InvalidEmail
    TeamInvitationNotAllowedForEmail -> Wai.mkError status403 "condition-failed" "Emails from this domain are not allowed to be invited to this team"
