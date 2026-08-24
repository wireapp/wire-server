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

import Data.Code
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Range
import Data.Text.Ascii (AsciiChars (validate), encodeBase64Url)
import Data.Text.Ascii qualified as Ascii
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.UUID qualified as UUID
import Imports
import Network.Mail.Mime
import Polysemy
import Polysemy.Output
import Test.Hspec
import Wire.API.Locale
import Wire.API.User (InvitationCode (InvitationCode, fromInvitationCode))
import Wire.API.User.Activation
import Wire.API.User.Client (Client (..), ClientCapabilityList (..), ClientType (..))
import Wire.API.User.EmailAddress
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.EmailSubsystem (AppEvent (..))
import Wire.EmailSubsystem.Interpreter
import Wire.EmailSubsystem.Template
import Wire.EmailSubsystem.Templates.Team
import Wire.EmailSubsystem.Templates.User

teamOpts :: TeamOpts
teamOpts =
  TeamOpts
    { tInvitationUrl = "https://example.com/join/?team-code=${code}",
      tExistingUserInvitationUrl = "https://example.com/accept-invitation/?team-code=${code}",
      tActivationUrl = "https://example.com/verify/?key=${key}&code=${code}",
      tCreatorWelcomeUrl = "https://example.com/creator-welcome-website",
      tMemberWelcomeUrl = "https://example.com/member-welcome-website",
      tAppManagementUrl = "https://example.com/team-management-website"
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

-- | Insert the default locale into the map of other locales, giving the full
-- set of locales that ship templates.
byLocale :: Localised a -> [(Locale, a)]
byLocale l = Map.assocs $ uncurry Map.insert l.locDefault l.locOther

-------------------------------------------------------------------------------
-- Sample emails

-- | One sample email rendered from the on-disk templates: its label, the
-- template variables that were left unreplaced, the resulting 'Mail', and any
-- assertions specific to this email.
data EmailSample = EmailSample
  { sampleName :: String,
    sampleErrors :: [Text],
    sampleMail :: Mail,
    sampleChecks :: Expectation
  }

mkSample :: String -> ([Text], Mail) -> EmailSample
mkSample name (errs, mail) = EmailSample name errs mail (pure ())

recipient :: EmailAddress
recipient = fromJust $ emailAddressText "test@example.com"

sampleTeamId :: TeamId
sampleTeamId = Id (fromJust $ UUID.fromString "123e4567-e89b-12d3-a456-426614174000")

sampleTeamName :: Text
sampleTeamName = "mrs. team"

sampleUserName :: Name
sampleUserName = Name "mr. user"

sampleDate :: UTCTimeMillis
sampleDate = toUTCTimeMillis (UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0))

sampleActivationKey :: ActivationKey
sampleActivationKey = ActivationKey . Ascii.unsafeFromText $ "key"

sampleActivationCode :: ActivationCode
sampleActivationCode = ActivationCode . Ascii.unsafeFromText $ "code666"

sampleVerificationCode :: Value
sampleVerificationCode = Value . unsafeRange . Ascii.unsafeFromText $ "code666"

teamSamples :: TeamTemplates -> [EmailSample]
teamSamples templates =
  [ invitationSample
      "team invitation"
      templates.invitationEmail
      "https://example.com/join/?team-code=zZoMX0xs=",
    invitationSample
      "team invitation existing user"
      templates.existingUserInvitationEmail
      "https://example.com/accept-invitation/?team-code=ZoMX0xs=",
    mkSample "member welcome" . run . runOutputList @Text $
      renderMemberWelcomeMail recipient sampleTeamId sampleTeamName templates.memberWelcomeEmail branding,
    mkSample "new team owner welcome" . run . runOutputList @Text $
      renderNewTeamOwnerWelcomeEmail recipient sampleTeamId sampleTeamName sampleUserName templates.newTeamOwnerWelcomeEmail branding
  ]
    <> [ mkSample label . run . runOutputList @Text $
           renderAppEventEmail recipient (Name "admin") sampleTeamId templates.appEmails event branding
       | (label, event) <- appEvents
       ]

invitationSample :: String -> InvitationEmailTemplate -> Text -> EmailSample
invitationSample label tpl expectedUrl =
  EmailSample label errs mail $ do
    mail.mailFrom.addressEmail `shouldBe` fromEmail tpl.invitationEmailSender
    url `shouldBe` expectedUrl
  where
    (errs, (mail, url)) = run . runOutputList @Text $ renderInvitationEmail input tpl branding
    input =
      InvitationEmail
        { invTo = recipient,
          invTeamId = sampleTeamId,
          invInvCode = InvitationCode {fromInvitationCode = fromRight undefined (validate "ZoMX0xs=")},
          invInviter = fromJust $ emailAddressText "inviter@example.com"
        }

-- | The five app-event emails all render through 'renderAppEventEmail' from the
-- 'AppEmailTemplates' bundled into 'TeamTemplates'. Each event carries a
-- different set of substitution variables.
appEvents :: [(String, AppEvent)]
appEvents =
  [ ( "app creation email",
      NewAppCreated
        { actor = "Actor",
          appName = Name "app",
          date = sampleDate,
          permissions = "read, write",
          teamId = sampleTeamId,
          teamName = sampleTeamName
        }
    ),
    ( "app deletion email",
      AppDeleted
        { actor = "Actor",
          appName = Name "app",
          date = sampleDate,
          teamId = sampleTeamId,
          teamName = sampleTeamName
        }
    ),
    ( "app availability change email",
      AppAvailabilityChanged
        { actor = "Actor",
          appName = Name "app",
          date = sampleDate,
          newAvailability = "available",
          previousAvailability = "unavailable",
          teamId = sampleTeamId,
          teamName = sampleTeamName
        }
    ),
    ( "app metadata change email",
      AppMetadataChanged
        { actor = "Actor",
          date = sampleDate,
          newAppName = Name "new app",
          previousAppName = Name "old app",
          teamId = sampleTeamId,
          teamName = sampleTeamName
        }
    ),
    ( "app token change email",
      AppTokenChanged
        { actor = "Actor",
          appName = Name "app",
          date = sampleDate,
          teamId = sampleTeamId,
          teamName = sampleTeamName
        }
    )
  ]

userSamples :: Locale -> UserTemplates -> [EmailSample]
userSamples loc templates =
  [ mkSample "password reset email" . run . runOutputList @Text $
      renderPwResetMail recipient (mkPasswordResetKey (Id UUID.nil)) (PasswordResetCode $ encodeBase64Url "bar") templates.passwordResetEmail branding,
    mkSample "verification email" . run . runOutputList @Text $
      renderVerificationMail recipient sampleActivationKey sampleActivationCode templates.verificationEmail branding,
    mkSample "team deletion verification email" . run . runOutputList @Text $
      renderSecondFactorVerificationEmail recipient sampleVerificationCode templates.verificationTeamDeletionEmail branding,
    mkSample "scim token verification email" . run . runOutputList @Text $
      renderSecondFactorVerificationEmail recipient sampleVerificationCode templates.verificationScimTokenEmail branding,
    mkSample "login verification email" . run . runOutputList @Text $
      renderSecondFactorVerificationEmail recipient sampleVerificationCode templates.verificationLoginEmail branding,
    mkSample "new client email" . run . runOutputList @Text $
      renderNewClientEmail recipient sampleUserName loc sampleClient templates.newClientEmail branding,
    mkSample "account deletion email" . run . runOutputList @Text $
      renderDeletionEmail recipient sampleUserName deletionKey deletionCode templates.deletionEmail branding,
    mkSample "activation email" . run . runOutputList @Text $
      renderActivationMail recipient sampleUserName sampleActivationKey sampleActivationCode templates.activationEmail branding,
    mkSample "activation email update" . run . runOutputList @Text $
      renderActivationMail recipient sampleUserName sampleActivationKey sampleActivationCode templates.activationEmailUpdate branding,
    mkSample "team activation email" . run . runOutputList @Text $
      renderTeamActivationMail recipient sampleUserName "team-name" sampleActivationKey sampleActivationCode templates.teamActivationEmail branding
  ]
  where
    deletionKey = Key . unsafeRange . Ascii.unsafeFromText $ "ABCDEFGHIJKLMNOPQRST"
    deletionCode = Value . unsafeRange . Ascii.unsafeFromText $ "code123"

sampleClient :: Client
sampleClient =
  Client
    { clientId = ClientId 1,
      clientType = PermanentClientType,
      clientTime = sampleDate,
      clientClass = Nothing,
      clientLabel = Just "label",
      clientCookie = Nothing,
      clientModel = Just "model",
      clientCapabilities = ClientCapabilityList mempty,
      clientMLSPublicKeys = Map.empty,
      clientLastActive = Nothing
    }
