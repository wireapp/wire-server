{-# LANGUAGE RecordWildCards #-}

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

module Brig.Team.Email
  ( sendMemberWelcomeMail,
    sendNewTeamOwnerWelcomeEmail,
  )
where

import Brig.App
import Brig.Team.Template
import Data.Id (TeamId, idToText)
import Data.Text.Lazy (toStrict)
import Imports
import Network.Mail.Mime
import Polysemy
import Wire.API.User
import Wire.EmailSending
import Wire.EmailSubsystem.Template

sendMemberWelcomeMail :: (Member EmailSending r) => EmailAddress -> TeamId -> Text -> Maybe Locale -> (AppT r) ()
sendMemberWelcomeMail to tid teamName loc = do
  tpl <- memberWelcomeEmail . snd <$> teamTemplatesWithLocale loc
  branding <- asks (.templateBranding)
  liftSem $ sendMail $ renderMemberWelcomeMail to tid teamName tpl branding

sendNewTeamOwnerWelcomeEmail :: (Member EmailSending r) => EmailAddress -> TeamId -> Text -> Maybe Locale -> Name -> (AppT r) ()
sendNewTeamOwnerWelcomeEmail to tid teamName loc profileName = do
  tpl <- newTeamOwnerWelcomeEmail . snd <$> teamTemplatesWithLocale loc
  branding <- asks (.templateBranding)
  liftSem $ sendMail $ renderNewTeamOwnerWelcomeEmail to tid teamName profileName tpl branding

-------------------------------------------------------------------------------
-- Member Welcome Email

renderMemberWelcomeMail :: EmailAddress -> TeamId -> Text -> MemberWelcomeEmailTemplate -> TemplateBranding -> Mail
renderMemberWelcomeMail emailTo tid teamName MemberWelcomeEmailTemplate {..} branding =
  (emptyMail from)
    { mailTo = [to],
      mailHeaders =
        [ ("Subject", toStrict subj),
          ("X-Zeta-Purpose", "Welcome")
        ],
      mailParts = [[plainPart txt, htmlPart html]]
    }
  where
    from = Address (Just memberWelcomeEmailSenderName) (fromEmail memberWelcomeEmailSender)
    to = Address Nothing (fromEmail emailTo)
    txt = renderTextWithBranding memberWelcomeEmailBodyText replace branding
    html = renderHtmlWithBranding memberWelcomeEmailBodyHtml replace branding
    subj = renderTextWithBranding memberWelcomeEmailSubject replace branding
    replace "url" = memberWelcomeEmailUrl
    replace "email" = fromEmail emailTo
    replace "team_id" = idToText tid
    replace "team_name" = teamName
    replace x = x

-------------------------------------------------------------------------------
-- New Team Owner Welcome Email

renderNewTeamOwnerWelcomeEmail :: EmailAddress -> TeamId -> Text -> Name -> NewTeamOwnerWelcomeEmailTemplate -> TemplateBranding -> Mail
renderNewTeamOwnerWelcomeEmail emailTo tid teamName profileName NewTeamOwnerWelcomeEmailTemplate {..} branding =
  (emptyMail from)
    { mailTo = [to],
      mailHeaders =
        [ ("Subject", toStrict subj),
          ("X-Zeta-Purpose", "Welcome")
        ],
      mailParts = [[plainPart txt, htmlPart html]]
    }
  where
    from = Address (Just newTeamOwnerWelcomeEmailSenderName) (fromEmail newTeamOwnerWelcomeEmailSender)
    to = Address Nothing (fromEmail emailTo)
    txt = renderTextWithBranding newTeamOwnerWelcomeEmailBodyText replace branding
    html = renderHtmlWithBranding newTeamOwnerWelcomeEmailBodyHtml replace branding
    subj = renderTextWithBranding newTeamOwnerWelcomeEmailSubject replace branding
    replace "url" = newTeamOwnerWelcomeEmailUrl
    replace "email" = fromEmail emailTo
    replace "team_id" = idToText tid
    replace "team_name" = teamName
    replace "name" = profileName.fromName
    replace x = x
