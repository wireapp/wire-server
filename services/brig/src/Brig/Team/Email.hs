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
  ( InvitationEmail (..),
    CreatorWelcomeEmail (..),
    MemberWelcomeEmail (..),
    sendMemberWelcomeMail,
  )
where

import Brig.App
import Brig.Team.Template
import Data.Id (TeamId, idToText)
import Data.Text.Ascii qualified as Ascii
import Data.Text.Lazy (toStrict)
import Imports
import Network.Mail.Mime
import Polysemy
import Wire.API.User
import Wire.EmailSending
import Wire.EmailSubsystem.Template (TemplateBranding, renderHtmlWithBranding, renderTextWithBranding)

sendMemberWelcomeMail :: (Member EmailSending r) => EmailAddress -> TeamId -> Text -> Maybe Locale -> (AppT r) ()
sendMemberWelcomeMail to tid teamName loc = do
  tpl <- memberWelcomeEmail . snd <$> teamTemplatesWithLocale loc
  branding <- asks (.templateBranding)
  let mail = MemberWelcomeEmail to tid teamName
  liftSem $ sendMail $ renderMemberWelcomeMail mail tpl branding

-------------------------------------------------------------------------------
-- Creator Welcome Email

data CreatorWelcomeEmail = CreatorWelcomeEmail
  { cwTo :: !EmailAddress,
    cwTid :: !TeamId,
    cwTeamName :: !Text
  }

-------------------------------------------------------------------------------
-- Member Welcome Email

data MemberWelcomeEmail = MemberWelcomeEmail
  { mwTo :: !EmailAddress,
    mwTid :: !TeamId,
    mwTeamName :: !Text
  }

renderMemberWelcomeMail :: MemberWelcomeEmail -> MemberWelcomeEmailTemplate -> TemplateBranding -> Mail
renderMemberWelcomeMail MemberWelcomeEmail {..} MemberWelcomeEmailTemplate {..} branding =
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
    to = Address Nothing (fromEmail mwTo)
    txt = renderTextWithBranding memberWelcomeEmailBodyText replace branding
    html = renderHtmlWithBranding memberWelcomeEmailBodyHtml replace branding
    subj = renderTextWithBranding memberWelcomeEmailSubject replace branding
    replace "url" = memberWelcomeEmailUrl
    replace "email" = fromEmail mwTo
    replace "team_id" = idToText mwTid
    replace "team_name" = mwTeamName
    replace x = x
