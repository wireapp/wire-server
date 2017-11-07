{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Brig.Team.Template
    ( TeamTemplates              (..)
    , InvitationEmailTemplate    (..)
    , loadTeamTemplates

      -- * Re-exports
    , Template
    , renderText
    , renderHtml
    ) where

import Brig.Options
import Brig.Template
import Brig.Types
import Data.Monoid
import Data.Text (Text)

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
loadTeamTemplates o = readLocalesDir defLocale templates $ \fp ->
    TeamTemplates
        <$> (InvitationEmailTemplate tUrl
                <$> readTemplate (fp <> "/email/invitation-subject.txt")
                <*> readTemplate (fp <> "/email/invitation.txt")
                <*> readTemplate (fp <> "/email/invitation.html")
                <*> pure (emailSender gOptions)
                <*> readText (fp <> "/email/sender.txt"))
  where
    gOptions = general (emailSMS o)
    tOptions = team (emailSMS o)
    tUrl     = template $ tInvitationUrl tOptions

    defLocale = setDefaultLocale (optSettings o)
    templates = templateDir gOptions <> "/team"
