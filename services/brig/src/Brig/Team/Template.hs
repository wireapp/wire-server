{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Brig.Team.Template
    ( TeamTemplates              (..)
    , InvitationEmailTemplate    (..)
    , loadTeamTemplates
    ) where

import Brig.Options
import Brig.Template
import Brig.Types
import Data.Monoid
import Data.Text (Text)

import qualified Data.Text.Encoding as Text

data InvitationEmailTemplate = InvitationEmailTemplate
    { invitationEmailUrl        :: !Template
    , invitationEmailSubject    :: !Template
    , invitationEmailBodyText   :: !Template
    , invitationEmailBodyHtml   :: !Template
    , invitationEmailSender     :: !Email
    , invitationEmailSenderName :: !Text
    }

data TeamTemplates = TeamTemplates
    { invitationEmail :: !InvitationEmailTemplate
    }

loadTeamTemplates :: Opts -> IO (Localised TeamTemplates)
loadTeamTemplates o = readLocalesDir defLocale templateDir $ \fp ->
    TeamTemplates
        <$> (InvitationEmailTemplate invitationUrl
                <$> readTemplate (fp <> "/email/invitation-subject.txt")
                <*> readTemplate (fp <> "/email/invitation.txt")
                <*> readTemplate (fp <> "/email/invitation.html")
                <*> pure (optEmailSender o)
                <*> readText (fp <> "/email/sender.txt"))
  where
    invitationUrl = template . Text.decodeLatin1 $ optTeamInvitationUrl o

    defLocale = setDefaultLocale (optSettings o)
    templateDir = optTemplateDir o <> "/team"
