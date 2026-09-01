-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.SAMLEmailSubsystem.InterpreterSpec (spec) where

import Data.Default
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Tagged (Tagged)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO qualified as TL
import Data.UUID qualified as UUID
import Data.X509.CertificateStore qualified as X509
import Imports
import Network.Mail.Mime (Address (..), Mail (..), Part (..), PartContent (..))
import Polysemy
import Polysemy.Error (runError)
import Polysemy.Output
import Polysemy.State
import SAML2.WebSSO
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import System.Logger qualified as Logger
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Email.Parser (unsafeEmailAddress)
import URI.ByteString
import Wire.API.BackgroundJobs.Email (SendEmailRequest)
import Wire.API.Error (ErrorS)
import Wire.API.Error.Galley (GalleyError (TeamMemberNotFound, TeamNotFound))
import Wire.API.Locale
import Wire.API.Password
import Wire.API.Routes.Internal.Brig (IdpChangedNotification (..))
import Wire.API.Team.Member
import Wire.API.Team.Permission (fullPermissions)
import Wire.API.Team.Role (Role (..))
import Wire.API.User.EmailAddress (fromEmail)
import Wire.API.User.IdentityProvider
import Wire.EmailSending.Composer (EmailTemplates (..), composeEmail)
import Wire.EmailSending.Queueing (EmailQueueing (QueueEmail))
import Wire.EmailSubsystem qualified as Email
import Wire.EmailSubsystem.Interpreter
import Wire.EmailSubsystem.TemplateFixtures
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
import Prelude qualified

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
          <$> readTextPartFile ("idp-config-change_created_" <> lang <> ".txt")
          <*> readTextPartFile ("idp-config-change_deleted_" <> lang <> ".txt")
          <*> readTextPartFile ("idp-config-change_updated_" <> lang <> ".txt")
          <*> readTextPartFile ("idp-config-change_subject_" <> lang <> ".txt")

  enTextParts <- runIO $ createTextParts "en"
  deTextParts <- runIO $ createTextParts "de"
  let -- We don't test all locals such that we do not have to adjust this test
      -- for every new translation. So far, there are translations for German
      -- and English. There's none for Spanish (falls back to English).
      testLocals :: [(Locale, RenderedTextParts)] =
        flip zip ((replicate 5 enTextParts) ++ (replicate 2 deTextParts)) $
          parseLocalUnsafe <$> ["en", "en-EN", "en-GB", "es", "es-ES", "de", "de_DE"]
      parseLocalUnsafe = fromMaybe (error "Unknown locale") . parseLocale

  -- Run duplicated IO tasks here to save some time
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

          (mails, logs, _res) <- runInterpreters [storedUser'] teamMap $ do
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
          (mails, logs, _res) <- runInterpreters [storedUser'] teamMap $ do
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
                  & ( \idp ->
                        idp
                          { _idpMetadata =
                              idp._idpMetadata
                                { _edCertAuthnResponse = NE.fromList newCerts,
                                  _edIssuer =
                                    Issuer . either (error . show) Imports.id $
                                      parseURI strictURIParserOptions "https://new-issuer.example.com/realm",
                                  _edRequestURI =
                                    either (error . show) Imports.id $
                                      parseURI strictURIParserOptions "https://new-saml-endpoint.example.com/auth"
                                }
                          }
                    )
              storedUser' = patchStoredUser storedUser teamId userLocale uid
              notif = IdPUpdated uid idpOld' idpNew'
          (mails, logs, _res) <- runInterpreters [storedUser'] teamMap $ do
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

          (mails, logs, _res) <- runInterpreters [storedUser'] teamMap $ do
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

          (mails, logs, _res) <- runInterpreters [storedUser'] teamMap $ do
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

          (mails, logs, _res) <- runInterpreters (fst <$> users) teamMap $ do
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
  Sem
    '[ SAMLEmailSubsystem,
       TeamSubsystem,
       Email.EmailSubsystem,
       UserStore,
       State [StoredUser],
       State (Map UserId Password),
       GalleyAPIAccess,
       Logger (Logger.Msg -> Logger.Msg),
       Output SendEmailRequest,
       ErrorS 'TeamMemberNotFound,
       ErrorS 'TeamNotFound,
       Embed IO
     ]
    a ->
  IO ([Mail], [(Level, LByteString)], a)
runInterpreters users teamMap action = do
  lr <- newLogRecorder
  (reqs, res) <-
    runM
      . fmap (either (error . show) (either (error . show) Imports.id))
      . runError @(Tagged 'TeamNotFound ())
      . runError @(Tagged 'TeamMemberNotFound ())
      . runOutputList @SendEmailRequest
      . recordLogs lr
      . miniGalleyAPIAccess teamMap def
      . evalState @(Map UserId Password) mempty
      . evalState @[StoredUser] users
      . inMemoryUserStoreInterpreter
      . emailSubsystemToOutput
      . interpretTeamSubsystemToGalleyAPI
      . samlEmailSubsystemInterpreter
      $ action
  logs <- readIORef lr.recordedLogs
  let (errs, mails) = run . runOutputList @Text $ traverse (composeEmail emailTemplatesFixture) reqs
  errs `shouldBe` []
  pure (mails, logs, res)

-- | Templates used to compose the recorded requests into mails. Loaded once
-- (the test suite runs with the package directory as working directory).
emailTemplatesFixture :: EmailTemplates
emailTemplatesFixture = unsafePerformIO $ do
  user <- loadTestUserTemplates
  teamTpls <- loadTestTeamTemplates
  provider <- loadTestProviderTemplates
  pure
    EmailTemplates
      { userTemplates = user,
        teamTemplates = teamTpls,
        providerTemplates = provider,
        brandingFn = Prelude.id,
        brandingMap = branding
      }
{-# NOINLINE emailTemplatesFixture #-}

-- | Interpret 'EmailSubsystem' by enqueueing into the 'Output' effect, so the
-- recorded requests can be composed to mails afterwards.
emailSubsystemToOutput ::
  (Member (Output SendEmailRequest) r) =>
  Sem (Email.EmailSubsystem : r) a ->
  Sem r a
emailSubsystemToOutput =
  interpret @EmailQueueing (\case QueueEmail req -> Polysemy.Output.output req)
    . emailSubsystemInterpreter
    . raiseUnder @EmailQueueing
