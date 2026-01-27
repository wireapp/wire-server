module Wire.SAMLEmailSubsystem.InterpreterSpec (spec) where

import Data.Default
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO qualified as TL
import Data.UUID qualified as UUID
import Imports
import Network.Mail.Mime (Address (..), Mail (..), Part (..), PartContent (..))
import Polysemy
import Polysemy.State
import SAML2.WebSSO
import System.Logger qualified as Logger
import Test.Hspec
import Test.QuickCheck (Arbitrary (arbitrary), generate, suchThat)
import Text.Email.Parser (unsafeEmailAddress)
import URI.ByteString
import Wire.API.Locale
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

-- TODO tests:
-- - Other local (found)
-- - Other local (not found)
-- - Update, Delete
spec :: Spec
spec = do
  let testLocals :: [Locale] = fromMaybe (error "Unknown locale") . parseLocale <$> ["en", "en_EN", "en_GB", "es", "es_ES"]
  describe "SendSAMLIdPChanged" $ do
    it "should send an email on IdPCreated" $ forM_ testLocals $ \(userLocale :: Locale) -> do
      idp :: IdP <- generate arbitrary
      storedUser :: StoredUser <- generate $ arbitrary `suchThat` (isJust . (.email))
      let teamOpts =
            TeamOpts
              { tInvitationUrl = "https://example.com/join/?team-code=${code}",
                tExistingUserInvitationUrl = "https://example.com/accept-invitation/?team-code=${code}",
                tActivationUrl = "https://example.com/verify/?key=${key}&code=${code}",
                tCreatorWelcomeUrl = "https://example.com/creator-welcome-website",
                tMemberWelcomeUrl = "https://example.com/member-welcome-website"
              }
          defLocale = Locale ((fromJust . parseLanguage) "en") Nothing
          emailSender = unsafeEmailAddress "wire" "example.com"
      teamTemplates <- loadTeamTemplates teamOpts "templates" defLocale emailSender
      let notif = IdPCreated (Just uid) idp'
          uid :: UserId = either error Imports.id $ parseIdFromText "4a1ce4ea-5c99-d01e-018f-4dc9d08f787a"
          teamId :: TeamId = either error Imports.id $ parseIdFromText "99f552d8-9dad-60c1-4be9-c88fb532893a"
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
                language = Just userLocale.lLanguage,
                country = userLocale.lCountry,
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
      englishCreateMailContent <- TL.stripEnd <$> TL.readFile "test/resources/mails/created_en.txt"
      case textPart.partContent of
        PartContent content -> (decodeUtf8 content) `shouldBe` englishCreateMailContent
        NestedParts ns -> error $ "Enexpected NestedParts: " ++ show ns

    it "should send an email on IdPDeleted" $ forM_ testLocals $ \(userLocale :: Locale) -> do
      idp :: IdP <- generate arbitrary
      storedUser :: StoredUser <- generate $ arbitrary `suchThat` (isJust . (.email))
      let teamOpts =
            TeamOpts
              { tInvitationUrl = "https://example.com/join/?team-code=${code}",
                tExistingUserInvitationUrl = "https://example.com/accept-invitation/?team-code=${code}",
                tActivationUrl = "https://example.com/verify/?key=${key}&code=${code}",
                tCreatorWelcomeUrl = "https://example.com/creator-welcome-website",
                tMemberWelcomeUrl = "https://example.com/member-welcome-website"
              }
          defLocale = Locale ((fromJust . parseLanguage) "en") Nothing
          emailSender = unsafeEmailAddress "wire" "example.com"
      teamTemplates <- loadTeamTemplates teamOpts "templates" defLocale emailSender
      let notif = IdPDeleted uid idp'
          uid :: UserId = either error Imports.id $ parseIdFromText "4a1ce4ea-5c99-d01e-018f-4dc9d08f787a"
          teamId :: TeamId = either error Imports.id $ parseIdFromText "99f552d8-9dad-60c1-4be9-c88fb532893a"
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
                language = Just userLocale.lLanguage,
                country = userLocale.lCountry,
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
      englishCreateMailContent <- TL.stripEnd <$> TL.readFile "test/resources/mails/deleted_en.txt"
      case textPart.partContent of
        PartContent content -> (decodeUtf8 content) `shouldBe` englishCreateMailContent
        NestedParts ns -> error $ "Enexpected NestedParts: " ++ show ns

-- | Records logs and mails
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
