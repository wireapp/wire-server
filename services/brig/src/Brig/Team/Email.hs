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
    sendInvitationMail,
    sendInvitationMailPersonalUser,
    sendMemberWelcomeMail,
  )
where

import Brig.App
import Brig.Team.Template
import Control.Lens (view)
import Data.Id (TeamId, idToText)
import Data.Text.Ascii qualified as Ascii
import Data.Text.Lazy (toStrict)
import Imports
import Network.Mail.Mime
import Polysemy
import Wire.API.User
import Wire.EmailSending
import Wire.EmailSubsystem.Template (TemplateBranding, renderHtmlWithBranding, renderTextWithBranding)

sendInvitationMail :: (Member EmailSending r) => EmailAddress -> TeamId -> EmailAddress -> InvitationCode -> Maybe Locale -> (AppT r) ()
sendInvitationMail to tid from code loc = do
  tpl <- invitationEmail . snd <$> teamTemplates loc
  branding <- view templateBranding
  let mail = InvitationEmail to tid code from
  liftSem $ sendMail $ renderInvitationEmail mail tpl branding

sendMemberWelcomeMail :: (Member EmailSending r) => EmailAddress -> TeamId -> Text -> Maybe Locale -> (AppT r) ()
sendMemberWelcomeMail to tid teamName loc = do
  tpl <- memberWelcomeEmail . snd <$> teamTemplates loc
  branding <- view templateBranding
  let mail = MemberWelcomeEmail to tid teamName
  liftSem $ sendMail $ renderMemberWelcomeMail mail tpl branding

sendInvitationMailPersonalUser :: (Member EmailSending r) => EmailAddress -> TeamId -> EmailAddress -> InvitationCode -> Maybe Locale -> (AppT r) ()
sendInvitationMailPersonalUser to tid from code loc = do
  tpl <- existingUserInvitationEmail . snd <$> teamTemplates loc
  branding <- view templateBranding
  let mail = InvitationEmail to tid code from
  liftSem $ sendMail $ renderInvitationEmail mail tpl branding

-------------------------------------------------------------------------------
-- Invitation Email

data InvitationEmail = InvitationEmail
  { invTo :: !EmailAddress,
    invTeamId :: !TeamId,
    invInvCode :: !InvitationCode,
    invInviter :: !EmailAddress
  }

renderInvitationEmail :: InvitationEmail -> InvitationEmailTemplate -> TemplateBranding -> Mail
renderInvitationEmail InvitationEmail {..} InvitationEmailTemplate {..} branding =
  (emptyMail from)
    { mailTo = [to],
      mailHeaders =
        [ ("Subject", toStrict subj),
          ("X-Zeta-Purpose", "TeamInvitation"),
          ("X-Zeta-Code", Ascii.toText code)
        ],
      mailParts = [[plainPart txt, htmlPart html]]
    }
  where
    (InvitationCode code) = invInvCode
    from = Address (Just invitationEmailSenderName) (fromEmail invitationEmailSender)
    to = Address Nothing (fromEmail invTo)
    txt = renderTextWithBranding invitationEmailBodyText replace branding
    html = renderHtmlWithBranding invitationEmailBodyHtml replace branding
    subj = renderTextWithBranding invitationEmailSubject replace branding
    replace "url" = renderInvitationUrl invitationEmailUrl invTeamId invInvCode branding
    replace "inviter" = fromEmail invInviter
    replace x = x

renderInvitationUrl :: Template -> TeamId -> InvitationCode -> TemplateBranding -> Text
renderInvitationUrl t tid (InvitationCode c) branding =
  toStrict $ renderTextWithBranding t replace branding
  where
    replace "team" = idToText tid
    replace "code" = Ascii.toText c
    replace x = x

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
