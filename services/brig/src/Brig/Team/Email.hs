{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Brig.Team.Email
    ( InvitationEmail     (..)
    , CreatorWelcomeEmail (..)
    , MemberWelcomeEmail  (..)
    , sendInvitationMail
    , sendCreatorWelcomeMail
    , sendMemberWelcomeMail
    ) where

import Brig.App
import Brig.Email
import Brig.Team.Template
import Brig.Types
import Data.Id (idToText, TeamId)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)

import qualified Brig.Aws        as Aws
import qualified Data.Text.Ascii as Ascii
-------------------------------------------------------------------------------
-- Invitation Email

sendInvitationMail :: Email -> TeamId -> Email -> InvitationCode -> Maybe Locale -> AppIO ()
sendInvitationMail to tid from code loc = do
    tpl <- invitationEmail . snd <$> teamTemplates loc
    let mail = InvitationEmail to tid code from
    Aws.sendMail $ renderInvitationEmail mail tpl

sendCreatorWelcomeMail :: Email -> TeamId -> Text -> Maybe Locale -> AppIO ()
sendCreatorWelcomeMail to tid teamName loc = do
    tpl <- creatorWelcomeEmail . snd <$> teamTemplates loc
    let mail = CreatorWelcomeEmail to tid teamName
    Aws.sendMail $ renderCreatorWelcomeMail mail tpl

sendMemberWelcomeMail :: Email -> TeamId -> Text -> Maybe Locale -> AppIO ()
sendMemberWelcomeMail to tid teamName loc = do
    tpl <- memberWelcomeEmail . snd <$> teamTemplates loc
    let mail = MemberWelcomeEmail to tid teamName
    Aws.sendMail $ renderMemberWelcomeMail mail tpl

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

-------------------------------------------------------------------------------
-- Creator Welcome Email

data CreatorWelcomeEmail = CreatorWelcomeEmail
    { cwTo       :: !Email
    , cwTid      :: !TeamId
    , cwTeamName :: !Text
    }

renderCreatorWelcomeMail :: CreatorWelcomeEmail -> CreatorWelcomeEmailTemplate -> Mail
renderCreatorWelcomeMail CreatorWelcomeEmail{..} CreatorWelcomeEmailTemplate{..} =
    (emptyMail from)
        { mailTo      = [ to ]
        , mailHeaders = [ ("Subject", toStrict subj)
                        , ("X-Zeta-Purpose", "Welcome")
                        ]
        , mailParts   = [ [ plainPart txt, htmlPart html ] ]
        }
  where
    from = Address (Just creatorWelcomeEmailSenderName) (fromEmail creatorWelcomeEmailSender)
    to   = Address Nothing (fromEmail cwTo)
    txt  = renderText creatorWelcomeEmailBodyText replace
    html = renderHtml creatorWelcomeEmailBodyHtml replace
    subj = renderText creatorWelcomeEmailSubject replace

    replace "url"       = creatorWelcomeEmailUrl
    replace "email"     = fromEmail cwTo
    replace "team_id"   = idToText cwTid
    replace "team_name" = cwTeamName
    replace x           = x

-------------------------------------------------------------------------------
-- Member Welcome Email

data MemberWelcomeEmail = MemberWelcomeEmail
    { mwTo       :: !Email
    , mwTid      :: !TeamId
    , mwTeamName :: !Text
    }

renderMemberWelcomeMail :: MemberWelcomeEmail -> MemberWelcomeEmailTemplate -> Mail
renderMemberWelcomeMail MemberWelcomeEmail{..} MemberWelcomeEmailTemplate{..} =
    (emptyMail from)
        { mailTo      = [ to ]
        , mailHeaders = [ ("Subject", toStrict subj)
                        , ("X-Zeta-Purpose", "Welcome")
                        ]
        , mailParts   = [ [ plainPart txt, htmlPart html ] ]
        }
  where
    from = Address (Just memberWelcomeEmailSenderName) (fromEmail memberWelcomeEmailSender)
    to   = Address Nothing (fromEmail mwTo)
    txt  = renderText memberWelcomeEmailBodyText replace
    html = renderHtml memberWelcomeEmailBodyHtml replace
    subj = renderText memberWelcomeEmailSubject replace

    replace "url"       = memberWelcomeEmailUrl
    replace "email"     = fromEmail mwTo
    replace "team_id"   = idToText mwTid
    replace "team_name" = mwTeamName
    replace x           = x
