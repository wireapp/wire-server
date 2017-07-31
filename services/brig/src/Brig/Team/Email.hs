{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Brig.Team.Email
    ( InvitationEmail (..)
    , sendInvitationMail
    ) where

import Brig.App
import Brig.Email
import Brig.Team.Template
import Brig.Types
import Data.Id (idToText, TeamId)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Template

import qualified Brig.Aws        as Aws
import qualified Data.Text.Ascii as Ascii
-------------------------------------------------------------------------------
-- Invitation Email

sendInvitationMail :: Email -> TeamId -> Name -> Text -> InvitationCode -> Maybe Locale -> AppIO ()
sendInvitationMail email tid inviterName teamName code loc = do
    tpl <- invitationEmail . snd <$> teamTemplates loc
    let mail = InvitationEmail email tid code inviterName teamName
    Aws.sendMail $ renderInvitationEmail mail tpl

data InvitationEmail = InvitationEmail
    { invTo          :: !Email
    , invTeamId      :: !TeamId
    , invInvCode     :: !InvitationCode
    , invInviterName :: !Name
    , invTeam        :: !Text
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
    to   = mkMimeAddress invInviterName invTo
    txt  = render invitationEmailBodyText replace
    html = render invitationEmailBodyHtml replace
    subj = render invitationEmailSubject  replace

    replace "url"      = renderInvitationUrl invitationEmailUrl invTeamId invInvCode
    replace "email"    = fromEmail invTo
    replace "inviter"  = fromName invInviterName
    replace "team"     = invTeam
    replace x          = x

renderInvitationUrl :: Template -> TeamId -> InvitationCode -> Text
renderInvitationUrl t tid (InvitationCode c) =
    toStrict $ render t replace
  where
    replace "team" = idToText tid
    replace "code" = Ascii.toText c
    replace x      = x
