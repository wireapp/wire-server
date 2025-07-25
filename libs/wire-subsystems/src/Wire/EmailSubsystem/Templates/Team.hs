{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.EmailSubsystem.Templates.Team where

import Data.Text.Template
import Imports hiding (readFile)
import Wire.API.User

data InvitationEmailTemplate = InvitationEmailTemplate
  { invitationEmailUrl :: !Text,
    invitationEmailSubject :: !Text,
    invitationEmailBodyText :: !Text,
    invitationEmailBodyHtml :: !Text,
    invitationEmailSender :: !EmailAddress,
    invitationEmailSenderName :: !Text
  }
  deriving (Show)

data CreatorWelcomeEmailTemplate = CreatorWelcomeEmailTemplate
  { creatorWelcomeEmailUrl :: !Text,
    creatorWelcomeEmailSubject :: !Template,
    creatorWelcomeEmailBodyText :: !Template,
    creatorWelcomeEmailBodyHtml :: !Template,
    creatorWelcomeEmailSender :: !EmailAddress,
    creatorWelcomeEmailSenderName :: !Text
  }
  deriving (Show)

data MemberWelcomeEmailTemplate = MemberWelcomeEmailTemplate
  { memberWelcomeEmailUrl :: !Text,
    memberWelcomeEmailSubject :: !Template,
    memberWelcomeEmailBodyText :: !Template,
    memberWelcomeEmailBodyHtml :: !Template,
    memberWelcomeEmailSender :: !EmailAddress,
    memberWelcomeEmailSenderName :: !Text
  }
  deriving (Show)

data NewTeamOwnerWelcomeEmailTemplate = NewTeamOwnerWelcomeEmailTemplate
  { newTeamOwnerWelcomeEmailUrl :: !Text,
    newTeamOwnerWelcomeEmailSubject :: !Template,
    newTeamOwnerWelcomeEmailBodyText :: !Template,
    newTeamOwnerWelcomeEmailBodyHtml :: !Template,
    newTeamOwnerWelcomeEmailSender :: !EmailAddress,
    newTeamOwnerWelcomeEmailSenderName :: !Text
  }
  deriving (Show)

data TeamTemplates = TeamTemplates
  { invitationEmail :: !InvitationEmailTemplate,
    existingUserInvitationEmail :: !InvitationEmailTemplate,
    creatorWelcomeEmail :: !CreatorWelcomeEmailTemplate,
    memberWelcomeEmail :: !MemberWelcomeEmailTemplate,
    newTeamOwnerWelcomeEmail :: !NewTeamOwnerWelcomeEmailTemplate
  }
  deriving (Show)
