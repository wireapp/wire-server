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

module Brig.User.Template (loadUserTemplates) where

import Brig.Options qualified as Opt
import Imports
import Wire.EmailSubsystem.Template hiding (loadUserTemplates)
import Wire.EmailSubsystem.Template qualified as EmailTemplate
import Wire.EmailSubsystem.Templates.User

loadUserTemplates :: Opt.Opts -> IO (Localised UserTemplates)
loadUserTemplates o =
  EmailTemplate.loadUserTemplates
    userTemplateOpts
    o.emailSMS.general.templateDir
    (Opt.defaultTemplateLocale o.settings)
    o.emailSMS.general.emailSender
  where
    userTemplateOpts =
      UserTemplateOpts
        { activationUrl = o.emailSMS.user.activationUrl,
          teamActivationUrl = o.emailSMS.team.tActivationUrl,
          passwordResetUrl = o.emailSMS.user.passwordResetUrl,
          deletionUrl = o.emailSMS.user.deletionUrl
        }
