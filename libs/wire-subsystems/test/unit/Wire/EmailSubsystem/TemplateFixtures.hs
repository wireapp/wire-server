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

-- | Shared fixtures for tests that render email templates from the on-disk
-- templates shipped with this package. The URL templates and branding here
-- mirror the shape of the corresponding Brig configuration.
module Wire.EmailSubsystem.TemplateFixtures where

import Data.Map qualified as Map
import Imports
import Text.Email.Parser (unsafeEmailAddress)
import Wire.API.Locale
import Wire.API.User.EmailAddress (EmailAddress)
import Wire.EmailSending.Composer (EmailTemplates (..))
import Wire.EmailSubsystem.Template hiding (emailSender)
import Wire.EmailSubsystem.Templates.Provider
import Wire.EmailSubsystem.Templates.Team
import Wire.EmailSubsystem.Templates.User

teamOpts :: TeamOpts
teamOpts =
  TeamOpts
    { tInvitationUrl = "https://example.com/join/?team-code=${code}",
      tExistingUserInvitationUrl = "https://example.com/accept-invitation/?team-code=${code}",
      tActivationUrl = "https://example.com/verify/?key=${key}&code=${code}",
      tCreatorWelcomeUrl = "https://example.com/creator-welcome-website",
      tMemberWelcomeUrl = "https://example.com/member-welcome-website"
    }

userTemplateOpts :: UserTemplateOpts
userTemplateOpts =
  UserTemplateOpts
    { activationUrl = "https://example.com/verify/?key=${key}&code=${code}",
      teamActivationUrl = teamOpts.tActivationUrl,
      passwordResetUrl = "https://example.com/reset/?key=${key}&code=${code}",
      deletionUrl = "https://example.com/d/?key=${key}&code=${code}"
    }

defLocale :: Locale
defLocale = Locale ((fromJust . parseLanguage) "en") Nothing

emailSender :: EmailAddress
emailSender = unsafeEmailAddress "wire" "example.com"

branding :: Map Text Text
branding =
  Map.fromList
    [ ("brand", "Wire Test"),
      ("brand_url", "https://wire.example.com"),
      ("brand_label_url", "wire.example.com"),
      ("brand_logo", "https://wire.example.com/p/img/email/logo-email-black.png"),
      ("brand_service", "Wire Service Provider"),
      ("copyright", "© WIRE SWISS GmbH"),
      ("misuse", "misuse@wire.example.com"),
      ("legal", "https://wire.example.com/legal/"),
      ("forgot", "https://wire.example.com/forgot/"),
      ("support", "https://support.wire.com/")
    ]

-- | Load the on-disk team templates. Relies on the test suite running with the
-- package directory as its working directory (as @cabal test@ does).
loadTestTeamTemplates :: IO (Localised TeamTemplates)
loadTestTeamTemplates = loadTeamTemplates teamOpts "templates" defLocale emailSender

-- | Load the on-disk user templates. See 'loadTestTeamTemplates'.
loadTestUserTemplates :: IO (Localised UserTemplates)
loadTestUserTemplates = loadUserTemplates userTemplateOpts "templates" defLocale emailSender

providerOpts :: ProviderOpts
providerOpts =
  ProviderOpts
    { homeUrl = "https://example.com/",
      providerActivationUrl = "https://example.com/provider-activate/?key=${key}&code=${code}",
      approvalUrl = "https://example.com/provider-approve/?key=${key}&code=${code}",
      approvalTo = emailSender,
      providerPwResetUrl = "https://example.com/provider-reset/?key=${key}&code=${code}"
    }

-- | Load the on-disk provider templates. See 'loadTestTeamTemplates'.
loadTestProviderTemplates :: IO (Localised ProviderTemplates)
loadTestProviderTemplates = loadProviderTemplates providerOpts "templates" defLocale emailSender

-- | Load the full composer fixture set (all template bundles plus branding).
loadTestEmailTemplates :: IO EmailTemplates
loadTestEmailTemplates = do
  user <- loadTestUserTemplates
  team <- loadTestTeamTemplates
  provider <- loadTestProviderTemplates
  pure
    EmailTemplates
      { userTemplates = user,
        teamTemplates = team,
        providerTemplates = provider,
        brandingFn = id,
        brandingMap = branding
      }
