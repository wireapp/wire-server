module Wire.TeamInvitationSubsystem.Error where

import Imports
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.Error

data TeamInvitationSubsystemError
  = TeamInvitationNoEmail
  | TeamInvitationInsufficientTeamPermissions
  | TooManyTeamInvitations
  | TeamInvitationBlacklistedEmail
  | TeamInvitationEmailTaken
  deriving (Show)

teamInvitationErrorToHttpError :: TeamInvitationSubsystemError -> HttpError
teamInvitationErrorToHttpError =
  StdError . \case
    TeamInvitationNoEmail -> errorToWai @E.NoEmail
    TeamInvitationInsufficientTeamPermissions -> errorToWai @E.InsufficientTeamPermissions
    TooManyTeamInvitations -> errorToWai @E.TooManyTeamInvitations
    TeamInvitationBlacklistedEmail -> errorToWai @E.BlacklistedEmail
    TeamInvitationEmailTaken -> errorToWai @E.EmailExists
