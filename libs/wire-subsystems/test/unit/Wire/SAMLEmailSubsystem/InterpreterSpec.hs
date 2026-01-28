module Wire.SAMLEmailSubsystem.InterpreterSpec (spec) where

import Data.Default
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO qualified as TL
import Data.UUID qualified as UUID
import Data.X509.CertificateStore qualified as X509
import Imports
import Network.Mail.Mime (Address (..), Mail (..), Part (..), PartContent (..))
import Polysemy
import Polysemy.State
import SAML2.WebSSO
import System.FilePath
import System.Logger qualified as Logger
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Email.Parser (unsafeEmailAddress)
import URI.ByteString
import Wire.API.Locale
import Wire.API.Routes.Internal.Brig (IdpChangedNotification (..))
import Wire.API.Team.Member
import Wire.API.Team.Permission (fullPermissions)
import Wire.API.Team.Role (Role (..))
import Wire.API.User.EmailAddress (fromEmail)
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
import Wire.StoredUser (StoredUser (..))
import Wire.TeamSubsystem
import Wire.TeamSubsystem.GalleyAPI (interpretTeamSubsystemToGalleyAPI)
import Wire.UserStore

data RenderedTextParts = RenderedTextParts
  { created :: LText,
    deleted :: LText,
    updated :: LText,
    subject :: LText
  }

spec :: Spec
spec = do
  let createTextParts lang =
        RenderedTextParts
          <$> readTextPartFile ("created_" <> lang <> ".txt")
          <*> readTextPartFile ("deleted_" <> lang <> ".txt")
          <*> readTextPartFile ("updated_" <> lang <> ".txt")
          <*> readTextPartFile ("subject_" <> lang <> ".txt")

  enTextParts <- runIO $ createTextParts "en"
  deTextParts <- runIO $ createTextParts "de"
  let -- We don't test all locals such that we do not have to adjust this test
      -- for every new translation. So far, there are translations for German
      -- and English. There's none for Spanish (falls back to English).
      testLocals :: [(Locale, RenderedTextParts)] =
        flip zip ((replicate 5 enTextParts) ++ (replicate 2 deTextParts)) $
          parseLocalUnsafe <$> ["en", "en-EN", "en-GB", "es", "es-ES", "de", "de_DE"]
      parseLocalUnsafe = fromMaybe (error "Unknown locale") . parseLocale
      teamOpts =
        TeamOpts
          { tInvitationUrl = "https://example.com/join/?team-code=${code}",
            tExistingUserInvitationUrl = "https://example.com/accept-invitation/?team-code=${code}",
            tActivationUrl = "https://example.com/verify/?key=${key}&code=${code}",
            tCreatorWelcomeUrl = "https://example.com/creator-welcome-website",
            tMemberWelcomeUrl = "https://example.com/member-welcome-website"
          }
      defLocale = Locale ((fromJust . parseLanguage) "en") Nothing
      emailSender = unsafeEmailAddress "wire" "example.com"
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

  -- Run duplicated IO tasks here to save some time
  teamTemplates :: Localised TeamTemplates <- runIO $ loadTeamTemplates teamOpts "templates" defLocale emailSender
  newCerts <- runIO $ X509.readCertificates "test/resources/saml/certs.store"

  describe "SendSAMLIdPChanged" $ do
    describe "localized emails" $ forM_ testLocals $ \(userLocale :: Locale, textParts) -> do
      let uid :: UserId = either error Imports.id $ parseIdFromText "4a1ce4ea-5c99-d01e-018f-4dc9d08f787a"
          teamId :: TeamId = either error Imports.id $ parseIdFromText "99f552d8-9dad-60c1-4be9-c88fb532893a"
          teamMember :: TeamMember = mkTeamMember uid fullPermissions Nothing UserLegalHoldDisabled
          teamMap :: Map TeamId [TeamMember] = Map.singleton teamId [teamMember]
      context ("locale: " ++ show userLocale) do
        it "should send an email on IdPCreated" $ do
          idp :: IdP <- liftIO $ generate arbitrary
          storedUser :: StoredUser <- liftIO . generate $ arbitrary `suchThat` (isJust . (.email))
          let idp' = patchIdP idp teamId
              storedUser' = patchStoredUser storedUser teamId userLocale uid
              notif = IdPCreated (Just uid) idp'

          (mails, logs, _res) <- runInterpreters [storedUser'] teamMap teamTemplates branding $ do
            sendSAMLIdPChanged notif

          assertNoWarnLogs logs

          length mails `shouldBe` 1
          let mail = head mails
          assertCommonMailAttributes mail textParts.subject
          assertMailTextPartWithFile mail textParts.created

        it "should send an email on IdPDeleted" $ do
          idp :: IdP <- liftIO $ generate arbitrary
          storedUser :: StoredUser <- liftIO . generate $ arbitrary `suchThat` (isJust . (.email))
          let idp' = patchIdP idp teamId
              storedUser' = patchStoredUser storedUser teamId userLocale uid
              notif = IdPDeleted uid idp'
          (mails, logs, _res) <- runInterpreters [storedUser'] teamMap teamTemplates branding $ do
            sendSAMLIdPChanged notif

          assertNoWarnLogs logs

          length mails `shouldBe` 1
          let mail = head mails
          assertCommonMailAttributes mail textParts.subject
          assertMailTextPartWithFile mail textParts.deleted

        it "should send an email on IdPUpdated" $ do
          idpOld :: IdP <- liftIO $ generate arbitrary
          idpNew :: IdP <- liftIO $ generate arbitrary
          storedUser :: StoredUser <- liftIO . generate $ arbitrary `suchThat` (isJust . (.email))
          let idpOld' = patchIdP idpOld teamId
              idpNew' =
                (patchIdP idpNew teamId)
                  { _idpMetadata =
                      idpNew._idpMetadata
                        { _edCertAuthnResponse = NE.fromList newCerts
                        }
                  }
              storedUser' = patchStoredUser storedUser teamId userLocale uid
              notif = IdPUpdated uid idpOld' idpNew'
          (mails, logs, _res) <- runInterpreters [storedUser'] teamMap teamTemplates branding $ do
            sendSAMLIdPChanged notif

          assertNoWarnLogs logs

          length mails `shouldBe` 1
          let mail = head mails
          assertCommonMailAttributes mail textParts.subject
          assertMailTextPartWithFile mail textParts.updated

    describe "logic" $ do
      prop "should not send to non-management roles" $
        \idp (StoredUserWithEmail storedUser) (OtherTeamRole role) uid teamId -> do
          let idp' = patchIdP idp teamId
              storedUser' = patchStoredUser storedUser teamId (parseLocalUnsafe "en") uid
              notif = IdPCreated (Just uid) idp'
              teamMember :: TeamMember = mkTeamMember uid (rolePermissions role) Nothing UserLegalHoldDisabled
              teamMap :: Map TeamId [TeamMember] = Map.singleton teamId [teamMember]

          (mails, logs, _res) <- runInterpreters [storedUser'] teamMap teamTemplates branding $ do
            sendSAMLIdPChanged notif

          assertNoWarnLogs logs

          length mails `shouldBe` 0

      prop "should send to team managers" $
        \idp (StoredUserWithEmail storedUser) (TeamManagementRole role) uid teamId -> do
          let idp' = patchIdP idp teamId
              storedUser' = patchStoredUser storedUser teamId (parseLocalUnsafe "en") uid
              notif = IdPCreated (Just uid) idp'
              teamMember :: TeamMember = mkTeamMember uid (rolePermissions role) Nothing UserLegalHoldDisabled
              teamMap :: Map TeamId [TeamMember] = Map.singleton teamId [teamMember]

          (mails, logs, _res) <- runInterpreters [storedUser'] teamMap teamTemplates branding $ do
            sendSAMLIdPChanged notif

          assertNoWarnLogs logs

          length mails `shouldBe` 1

      prop ("can send to multiple receivers") $
        \idp (TestTeam tid users) uid -> do
          let idp' = patchIdP idp tid
              notif = IdPCreated (Just uid) idp'
              teamMap :: Map TeamId [TeamMember] = Map.singleton tid (snd <$> users)
              adminsAndOwners :: [(StoredUser, TeamMember)] =
                filter
                  ( \(_u, tm) ->
                      permissionsRole (Wire.API.Team.Member.getPermissions tm) `elem` (Just <$> teamManagementRoles)
                  )
                  users

          (mails, logs, _res) <- runInterpreters (fst <$> users) teamMap teamTemplates branding $ do
            sendSAMLIdPChanged notif

          assertNoWarnLogs logs

          length mails `shouldBe` length adminsAndOwners
          let receiverAddresses :: [Text] = addressEmail <$> concatMap (.mailTo) mails
              expectedAddresses :: [Text] = fromEmail . fromJust . email . fst <$> adminsAndOwners
          length receiverAddresses `shouldBe` length adminsAndOwners
          Set.fromList receiverAddresses `shouldBe` Set.fromList expectedAddresses

-- Templating issues are logged on level `Warn`
assertNoWarnLogs :: (Show b, Eq b) => [(Level, b)] -> Expectation
assertNoWarnLogs logs = filter (\(level, _) -> level > Info) logs `shouldBe` mempty

newtype OtherTeamRole = OtherTeamRole Role
  deriving (Show)

instance Arbitrary OtherTeamRole where
  arbitrary = OtherTeamRole <$> elements ([minBound .. maxBound] \\ [RoleAdmin, RoleOwner])

newtype TeamManagementRole = TeamManagementRole Role
  deriving (Show)

instance Arbitrary TeamManagementRole where
  arbitrary = TeamManagementRole <$> elements teamManagementRoles

teamManagementRoles :: [Role]
teamManagementRoles = [RoleAdmin, RoleOwner]

data TestTeam = TestTeam TeamId [(StoredUser, TeamMember)]
  deriving (Show)

instance Arbitrary TestTeam where
  arbitrary = do
    teamId :: TeamId <- arbitrary
    users :: [StoredUserWithEmail] <-
      (\(StoredUserWithEmail r) -> StoredUserWithEmail r {teamId = Just teamId})
        <$$> arbitrary
    teamMbrs <- mapM (\(StoredUserWithEmail u) -> makeTeamMember u) users
    pure $ TestTeam teamId (zip (getStoredUser <$> users) teamMbrs)
    where
      makeTeamMember :: StoredUser -> Gen TeamMember
      makeTeamMember user = do
        userRole :: Role <- arbitrary
        mkTeamMember user.id (rolePermissions userRole) <$> arbitrary <*> arbitrary

newtype StoredUserWithEmail = StoredUserWithEmail {getStoredUser :: StoredUser}
  deriving (Show)

instance Arbitrary StoredUserWithEmail where
  arbitrary =
    StoredUserWithEmail
      <$> arbitrary
        `suchThat` (isJust . (.email))

patchIdP :: IdPConfig WireIdP -> TeamId -> IdPConfig WireIdP
patchIdP idp teamId =
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

patchStoredUser :: StoredUser -> TeamId -> Locale -> UserId -> StoredUser
patchStoredUser storedUser teamId userLocale uid =
  (storedUser :: StoredUser)
    { id = uid,
      teamId = Just teamId,
      language = Just userLocale.lLanguage,
      country = userLocale.lCountry,
      email = Just $ unsafeEmailAddress "some-user" "example.com"
    }

readTextPartFile :: FilePath -> IO TL.Text
readTextPartFile file = TL.stripEnd <$> TL.readFile ("test" </> "resources" </> "mails" </> file)

assertCommonMailAttributes :: Mail -> LText -> IO ()
assertCommonMailAttributes mail expectedSubject = do
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
      [ ("Subject", TL.toStrict expectedSubject),
        ("X-Zeta-Purpose", "IdPConfigChange")
      ]

assertMailTextPartWithFile :: Mail -> LText -> IO ()
assertMailTextPartWithFile mail expectedTextPart = do
  let textPart =
        fromMaybe (error "No text part found") $
          find (\p -> p.partType == "text/plain; charset=utf-8") (head mail.mailParts)
  case textPart.partContent of
    PartContent content -> (decodeUtf8 content) `shouldBe` expectedTextPart
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
