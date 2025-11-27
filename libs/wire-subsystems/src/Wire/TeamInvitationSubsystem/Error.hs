-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

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
  | TeamInvitationBlockedDomain
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
    TeamInvitationBlockedDomain -> errorToWai @E.CustomerExtensionBlockedDomain
