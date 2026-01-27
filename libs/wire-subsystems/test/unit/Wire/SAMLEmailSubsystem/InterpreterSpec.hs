module Wire.SAMLEmailSubsystem.InterpreterSpec where

import Data.Default
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Template
import Data.UUID qualified as UUID
import Imports
import Network.Mail.Mime (Address (..), Disposition (..), Encoding (..), Mail (..), Part (..), PartContent (..))
import Polysemy
import Polysemy.State
import SAML2.WebSSO
import System.Logger qualified as Logger
import Test.Hspec
import Test.QuickCheck (Arbitrary (arbitrary), generate, suchThat)
import Text.Email.Parser (unsafeEmailAddress)
import Text.RawString.QQ (r)
import URI.ByteString
import Wire.API.Locale (Locale (..), parseLanguage)
import Wire.API.Routes.Internal.Brig (IdpChangedNotification (..))
import Wire.API.Team.Member
import Wire.API.Team.Permission (fullPermissions)
import Wire.API.User.IdentityProvider
import Wire.EmailSending
import Wire.EmailSubsystem qualified as Email
import Wire.EmailSubsystem.Interpreter
import Wire.EmailSubsystem.Template
import Wire.EmailSubsystem.Templates.Team
import Wire.GalleyAPIAccess
import Wire.MockInterpreters
import Wire.SAMLEmailSubsystem
import Wire.SAMLEmailSubsystem.Interpreter (samlEmailSubsystemInterpreter)
import Wire.Sem.Logger
import Wire.Sem.Logger.TinyLog
import Wire.StoredUser
import Wire.TeamSubsystem
import Wire.TeamSubsystem.GalleyAPI (interpretTeamSubsystemToGalleyAPI)
import Wire.UserStore

spec :: Spec
spec = do
  describe "SendSAMLIdPChanged" $ do
    it "should send an email" $ do
      idp :: IdP <- generate arbitrary
      storedUser :: StoredUser <- generate $ arbitrary `suchThat` (isJust . (.email))
      teamTemplates <- loadTeamTemplates
      let notif = IdPCreated (Just uid) idp'
          uid :: UserId = either error Imports.id $ parseIdFromText "4a1ce4ea-5c99-d01e-018f-4dc9d08f787a"
          teamId :: TeamId = either error Imports.id $ parseIdFromText "99f552d8-9dad-60c1-4be9-c88fb532893a"
          lang = parseLanguage "en"
          idp' =
            idp
              { _idpId = IdPId . fromJust . UUID.fromString $ "574ddfb0-4e50-2bff-e924-33ee2b9f7064",
                _idpMetadata =
                  idp._idpMetadata
                    { _edIssuer = Issuer . either (error . show) Imports.id $ parseURI strictURIParserOptions "https://issuer.example.com/realm",
                      _edRequestURI = either (error . show) Imports.id $ parseURI strictURIParserOptions "https://saml-endpoint.example.com/auth"
                    },
                _idpExtraInfo =
                  idp._idpExtraInfo
                    { _team = teamId
                    }
              }
          storedUser' =
            (storedUser :: StoredUser)
              { id = uid,
                teamId = Just teamId,
                language = lang,
                country = Nothing,
                email = Just $ unsafeEmailAddress "some-user" "example.com"
              }
          teamMember :: TeamMember = mkTeamMember uid fullPermissions Nothing UserLegalHoldDisabled
          teamMap :: Map TeamId [TeamMember] = Map.singleton teamId [teamMember]
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
      (mails, logs, _res) <- runInterpreters [storedUser'] teamMap teamTemplates branding $ do
        sendSAMLIdPChanged notif
      length mails `shouldBe` 1
      -- Templating issues are logged on level `Warn`
      filter (\(level, _) -> level > Info) logs `shouldBe` mempty
      let mail = head mails
      mail.mailFrom
        `shouldBe` Address
          { addressName = Just "Wire",
            addressEmail = "wire@example.com"
          }
      mail.mailTo
        `shouldBe` [ Address
                       { addressName = Nothing,
                         addressEmail = "some-user@example.com"
                       }
                   ]
      mail.mailCc `shouldBe` []
      mail.mailBcc `shouldBe` []
      Set.fromList mail.mailHeaders
        `shouldBe` Set.fromList
          [ ("Subject", "Your team&#x27;s identity provider configuration has changed"),
            ("X-Zeta-Purpose", "IdPConfigChange")
          ]
      let textPart =
            fromMaybe (error "No text part found") $
              find (\p -> p.partType == "text/plain; charset=utf-8") (head mail.mailParts)
      case textPart.partContent of
        PartContent content ->
          (decodeUtf8 content)
            `shouldBe` [r|[https://wire.example.com/p/img/email/logo-email-black.png]

wire.example.com [https://wire.example.com]

CHANGE IN YOUR IDENTITY PROVIDER CONFIGURATION
Something has changed in the IdP configuration for your team.

Team ID:
99f552d8-9dad-60c1-4be9-c88fb532893a

User ID:
4a1ce4ea-5c99-d01e-018f-4dc9d08f787a


--------------------------------------------------------------------------------

Details:

IdP Issuer:
https://issuer.example.com/realm

IdP Endpoint:
https://saml-endpoint.example.com/auth

IdP ID:
574ddfb0-4e50-2bff-e924-33ee2b9f7064


--------------------------------------------------------------------------------

Added:

SHA1 fingerprint:
15:28:A6:B8:5A:C5:36:80:B4:B0:95:C6:9A:FD:77:9C:D6:5C:78:37

Subject:
CN=accounts.accesscontrol.windows.net

Issuer:
CN=accounts.accesscontrol.windows.net


--------------------------------------------------------------------------------


If you did not initiate this change, please reach out to the Wire support.
[https://support.wire.com/]

Privacy Policy and Terms of Use [https://wire.example.com/legal/]· Report misuse [misuse@wire.example.com]
© WIRE SWISS GmbH. All rights reserved.|]
        NestedParts ns -> error $ "Enexpected NestedParts: " ++ show ns

runInterpreters ::
  [StoredUser] ->
  Map TeamId [TeamMember] ->
  Localised TeamTemplates ->
  Map Text Text ->
  Sem
    '[ SAMLEmailSubsystem,
       TeamSubsystem,
       Email.EmailSubsystem,
       UserStore,
       State [StoredUser],
       GalleyAPIAccess,
       Logger (Logger.Msg -> Logger.Msg),
       EmailSending,
       State [Mail],
       Embed IO
     ]
    a ->
  IO ([Mail], [(Level, LByteString)], a)
runInterpreters users teamMap teamTemplates branding action = do
  lr <- newLogRecorder
  (mails, (_userState, res)) <-
    runM
      . runState @[Mail] [] -- Use runState to capture and return the Mail state
      . recordingEmailSendingInterpreter
      . recordLogs lr
      . miniGalleyAPIAccess teamMap def
      . runState @[StoredUser] users
      . inMemoryUserStoreInterpreter
      . emailSubsystemInterpreter undefined teamTemplates branding
      . interpretTeamSubsystemToGalleyAPI
      . samlEmailSubsystemInterpreter
      $ action
  logs <- readIORef lr.recordedLogs
  pure (mails, logs, res)

loadTeamTemplates :: IO (Localised TeamTemplates)
loadTeamTemplates = readLocalesDir defLocale templateDir "team" $ \fp ->
  TeamTemplates
    <$> ( InvitationEmailTemplate tUrl
            <$> readTemplate fp "email/invitation-subject.txt"
            <*> readTemplate fp "email/invitation.txt"
            <*> readTemplate fp "email/invitation.html"
            <*> pure emailSender
            <*> readText fp "email/sender.txt"
        )
    <*> ( InvitationEmailTemplate tExistingUrl
            <$> readTemplate fp "email/migration-subject.txt"
            <*> readTemplate fp "email/migration.txt"
            <*> readTemplate fp "email/migration.html"
            <*> pure emailSender
            <*> readText fp "email/sender.txt"
        )
    <*> ( MemberWelcomeEmailTemplate memberWelcomeUrl
            <$> readTemplate fp "email/new-member-welcome-subject.txt"
            <*> readTemplate fp "email/new-member-welcome.txt"
            <*> readTemplate fp "email/new-member-welcome.html"
            <*> pure emailSender
            <*> readText fp "email/sender.txt"
        )
    <*> ( NewTeamOwnerWelcomeEmailTemplate creatorWelcomeUrl
            <$> readTemplate fp "email/new-team-owner-welcome-subject.txt"
            <*> readTemplate fp "email/new-team-owner-welcome.txt"
            <*> readTemplate fp "email/new-team-owner-welcome.html"
            <*> pure emailSender
            <*> readText fp "email/sender.txt"
        )
    <*>
    -- TODO: Template paths
    ( IdPConfigChangeEmailTemplate
        <$> readTemplate fp "../partials/idp-certificate-added.html"
        <*> readTemplate fp "../partials/idp-certificate-added.txt"
        <*> readTemplate fp "../partials/idp-certificate-removed.html"
        <*> readTemplate fp "../partials/idp-certificate-removed.txt"
        <*> readTemplate fp "email/idp-config-change-subject.txt"
        <*> readTemplate fp "email/idp-config-change.txt"
        <*> readTemplate fp "email/idp-config-change.html"
        <*> pure emailSender
        <*> readText fp "email/sender.txt"
    )
  where
    memberWelcomeUrl = "https://example.com/member-welcome-website"
    creatorWelcomeUrl = "https://example.com/creator-welcome-website"
    emailSender = unsafeEmailAddress "wire" "example.com"
    tUrl = template "https://example.com/join/?team-code=${code}"
    tExistingUrl = template "https://example.com/accept-invitation/?team-code=${code}"
    defLocale = Locale ((fromJust . parseLanguage) "en") Nothing
    readTemplate = readTemplateWithDefault templateDir defLocale "team"
    readText = readTextWithDefault templateDir defLocale "team"
    templateDir = "templates"
