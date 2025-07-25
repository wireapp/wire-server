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

module Brig.Team.Template
  ( TeamTemplates (..),
    InvitationEmailTemplate (..),
    CreatorWelcomeEmailTemplate (..),
    MemberWelcomeEmailTemplate (..),
    loadTeamTemplates,

    -- * Re-exports
    Template,
  )
where

import Brig.Options
import Brig.Template
import Imports
import Wire.EmailSubsystem.Template hiding (loadTeamTemplates)
import Wire.EmailSubsystem.Template qualified as Emails

loadTeamTemplates :: Opts -> IO (Localised TeamTemplates)
loadTeamTemplates o =
  Emails.loadTeamTemplates
    (defaultTemplateLocale o.settings)
    (templateDir o.emailSMS.general)
    (o.emailSMS.general.emailSender)
    (o.emailSMS.team.tInvitationUrl)
    (o.emailSMS.team.tExistingUserInvitationUrl)
    (o.emailSMS.team.tCreatorWelcomeUrl)
    (o.emailSMS.team.tMemberWelcomeUrl)
