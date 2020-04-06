-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
    renderText,
    renderHtml,
  )
where

import Brig.Options
import Brig.Template
import Brig.Types
import Imports

data InvitationEmailTemplate
  = InvitationEmailTemplate
      { invitationEmailUrl :: !Template,
        invitationEmailSubject :: !Template,
        invitationEmailBodyText :: !Template,
        invitationEmailBodyHtml :: !Template,
        invitationEmailSender :: !Email,
        invitationEmailSenderName :: !Text
      }

data CreatorWelcomeEmailTemplate
  = CreatorWelcomeEmailTemplate
      { creatorWelcomeEmailUrl :: !Text,
        creatorWelcomeEmailSubject :: !Template,
        creatorWelcomeEmailBodyText :: !Template,
        creatorWelcomeEmailBodyHtml :: !Template,
        creatorWelcomeEmailSender :: !Email,
        creatorWelcomeEmailSenderName :: !Text
      }

data MemberWelcomeEmailTemplate
  = MemberWelcomeEmailTemplate
      { memberWelcomeEmailUrl :: !Text,
        memberWelcomeEmailSubject :: !Template,
        memberWelcomeEmailBodyText :: !Template,
        memberWelcomeEmailBodyHtml :: !Template,
        memberWelcomeEmailSender :: !Email,
        memberWelcomeEmailSenderName :: !Text
      }

data TeamTemplates
  = TeamTemplates
      { invitationEmail :: !InvitationEmailTemplate,
        creatorWelcomeEmail :: !CreatorWelcomeEmailTemplate,
        memberWelcomeEmail :: !MemberWelcomeEmailTemplate
      }

loadTeamTemplates :: Opts -> IO (Localised TeamTemplates)
loadTeamTemplates o = readLocalesDir defLocale (templateDir gOptions) "team" $ \fp ->
  TeamTemplates
    <$> ( InvitationEmailTemplate tUrl
            <$> readTemplate fp "email/invitation-subject.txt"
            <*> readTemplate fp "email/invitation.txt"
            <*> readTemplate fp "email/invitation.html"
            <*> pure (emailSender gOptions)
            <*> readText fp "email/sender.txt"
        )
    <*> ( CreatorWelcomeEmailTemplate (tCreatorWelcomeUrl tOptions)
            <$> readTemplate fp "email/new-creator-welcome-subject.txt"
            <*> readTemplate fp "email/new-creator-welcome.txt"
            <*> readTemplate fp "email/new-creator-welcome.html"
            <*> pure (emailSender gOptions)
            <*> readText fp "email/sender.txt"
        )
    <*> ( MemberWelcomeEmailTemplate (tMemberWelcomeUrl tOptions)
            <$> readTemplate fp "email/new-member-welcome-subject.txt"
            <*> readTemplate fp "email/new-member-welcome.txt"
            <*> readTemplate fp "email/new-member-welcome.html"
            <*> pure (emailSender gOptions)
            <*> readText fp "email/sender.txt"
        )
  where
    gOptions = general (emailSMS o)
    tOptions = team (emailSMS o)
    tUrl = template $ tInvitationUrl tOptions
    defLocale = setDefaultLocale (optSettings o)
    readTemplate = readTemplateWithDefault (templateDir gOptions) defLocale "team"
    readText = readTextWithDefault (templateDir gOptions) defLocale "team"
