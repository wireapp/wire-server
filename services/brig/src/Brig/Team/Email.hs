{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Brig.Team.Email
    ( InvitationEmail     (..)
    , sendInvitationMail
    ) where

import Brig.App
import Brig.Email
import Brig.Team.Template
import Brig.Types
import Data.Id (idToText, TeamId)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)

import qualified Brig.Email      as Email
import qualified Data.Text.Ascii as Ascii
-------------------------------------------------------------------------------
-- Invitation Email

sendInvitationMail :: Email -> TeamId -> Email -> InvitationCode -> Maybe Locale -> AppIO ()
sendInvitationMail to tid from code loc = do
    tpl <- invitationEmail . snd <$> teamTemplates loc
    let mail = InvitationEmail to tid code from
    Email.sendMail $ renderInvitationEmail mail tpl

-------------------------------------------------------------------------------
-- Invitation Email

data InvitationEmail = InvitationEmail
    { invTo      :: !Email
    , invTeamId  :: !TeamId
    , invInvCode :: !InvitationCode
    , invInviter :: !Email
    }

renderInvitationEmail :: InvitationEmail -> InvitationEmailTemplate -> Mail
renderInvitationEmail InvitationEmail{..} InvitationEmailTemplate{..} =
    (emptyMail from)
        { mailTo      = [ to ]
        , mailHeaders = [ ("Subject", toStrict subj)
                        , ("X-Zeta-Purpose", "TeamInvitation")
                        , ("X-Zeta-Code", Ascii.toText code)
                        ]
        , mailParts   = [ [ plainPart txt, htmlPart html ] ]
        }
  where
    (InvitationCode code) = invInvCode

    from = Address (Just invitationEmailSenderName) (fromEmail invitationEmailSender)
    to   = Address Nothing (fromEmail invTo)
    txt  = renderText invitationEmailBodyText replace
    html = renderHtml invitationEmailBodyHtml replace
    subj = renderText invitationEmailSubject  replace

    replace "url"      = renderInvitationUrl invitationEmailUrl invTeamId invInvCode
    replace "inviter"  = fromEmail invInviter
    replace x          = x

renderInvitationUrl :: Template -> TeamId -> InvitationCode -> Text
renderInvitationUrl t tid (InvitationCode c) =
    toStrict $ renderText t replace
  where
    replace "team" = idToText tid
    replace "code" = Ascii.toText c
    replace x      = x
