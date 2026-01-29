module API.Template (tests) where

import Bilge
import Brig.Options
import Brig.Team.Template (loadTeamTemplatesWithBrigOpts)
import Brig.Template
import Brig.User.Template (loadUserTemplates)
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
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.API.Locale
import Wire.API.User (InvitationCode (InvitationCode, fromInvitationCode))
import Wire.API.User.Activation
import Wire.API.User.Client (Client (..), ClientCapabilityList (..), ClientType (..))
import Wire.API.User.EmailAddress
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.EmailSubsystem.Interpreter
import Wire.EmailSubsystem.Template
import Wire.EmailSubsystem.Templates.Team
import Wire.EmailSubsystem.Templates.User

-- FUTUREWORK: This does not have to be an integration test. It could be
-- covered by unit tests in wire-subsystems. Then, the helper functions can be
-- privatized.
tests :: Opts -> Manager -> IO TestTree
tests opts m = do
  team <- liftIO $ loadTeamTemplatesWithBrigOpts opts
  user <- liftIO $ loadUserTemplates opts
  let teamTemplates = Map.assocs $ uncurry Map.insert team.locDefault team.locOther
      userTemplates = Map.assocs $ uncurry Map.insert user.locDefault user.locOther
      b = genTemplateBrandingMap opts.emailSMS.general.templateBranding
  pure $
    testGroup
      "email templates"
      [ testGroup
          "team"
          $ fmap
            ( \(loc, ts) ->
                testGroup
                  (show loc)
                  [ test m "team invitation" $ testTeamInvitationEmail b ts,
                    test m "team invitation existing user" $ testTeamInvitationEmailExistingUser b ts,
                    test m "member welcome" $ testMemberWelcomeEmail b ts,
                    test m "new team owner welcome" $ testNewTeamOwnerWelcomeEmail b ts
                  ]
            )
            teamTemplates,
        testGroup "user" $
          fmap
            ( \(loc, ts) ->
                testGroup
                  (show loc)
                  [ test m "password reset email" $ testPasswordResetEmail b ts,
                    test m "verification email" $ testVerificationEmail b ts,
                    test m "team deletion verification email" $ testTeamDeletionVerificationEmail b ts,
                    test m "scim token verification email" $ testScimTokenVerificationEmail b ts,
                    test m "login verification email" $ testLoginVerificationEmail b ts,
                    test m "new client email" $ testNewClientEmail b loc ts,
                    test m "account deletion email" $ testAccountDeletionEmail b ts,
                    test m "activation email" $ testActivationEmail b ts,
                    test m "activation email update" $ testActivationEmailUpdate b ts,
                    test m "team activation email" $ testTeamActivationEmail b ts
                  ]
            )
            userTemplates
      ]

testTeamInvitationEmailExistingUser :: (HasCallStack) => Map Text Text -> TeamTemplates -> Http ()
testTeamInvitationEmailExistingUser branding templates = do
  let tpl = templates.existingUserInvitationEmail
      (errs, (mail, url)) = run $ runOutputList @Text $ renderInvitationEmail input tpl branding
      input =
        InvitationEmail
          { invTo = fromJust $ emailAddressText "test@example.com",
            invTeamId = Id (fromJust $ UUID.fromString "123e4567-e89b-12d3-a456-426614174000"),
            invInvCode = InvitationCode {fromInvitationCode = fromRight undefined (validate "ZoMX0xs=")},
            invInviter = fromJust $ emailAddressText "inviter@example.com"
          }
  liftIO $ mail.mailFrom.addressEmail @?= (fromEmail tpl.invitationEmailSender)
  liftIO $ url @?= "https://example.com/accept-invitation/?team-code=ZoMX0xs="
  assertNoErrors errs

testTeamInvitationEmail :: (HasCallStack) => Map Text Text -> TeamTemplates -> Http ()
testTeamInvitationEmail branding templates = do
  let tpl = templates.invitationEmail
      (errs, (mail, url)) = run $ runOutputList @Text $ renderInvitationEmail input tpl branding
      input =
        InvitationEmail
          { invTo = fromJust $ emailAddressText "test@example.com",
            invTeamId = Id (fromJust $ UUID.fromString "123e4567-e89b-12d3-a456-426614174000"),
            invInvCode = InvitationCode {fromInvitationCode = fromRight undefined (validate "ZoMX0xs=")},
            invInviter = fromJust $ emailAddressText "inviter@example.com"
          }
  liftIO $ mail.mailFrom.addressEmail @?= (fromEmail tpl.invitationEmailSender)
  liftIO $ url @?= "https://example.com/join/?team-code=ZoMX0xs="
  assertNoErrors errs

testMemberWelcomeEmail :: (HasCallStack) => Map Text Text -> TeamTemplates -> Http ()
testMemberWelcomeEmail branding templates = do
  let tpl = templates.memberWelcomeEmail
      to = fromJust $ emailAddressText "test@example.com"
      tid = Id (fromJust $ UUID.fromString "123e4567-e89b-12d3-a456-426614174000")
      tname = "funky team"
      (errs, _) = run $ runOutputList @Text $ renderMemberWelcomeMail to tid tname tpl branding
  assertNoErrors errs

testNewTeamOwnerWelcomeEmail :: (HasCallStack) => Map Text Text -> TeamTemplates -> Http ()
testNewTeamOwnerWelcomeEmail branding templates = do
  let tpl = templates.newTeamOwnerWelcomeEmail
      to = fromJust $ emailAddressText "test@example.com"
      tid = Id (fromJust $ UUID.fromString "123e4567-e89b-12d3-a456-426614174000")
      tname = "funky team"
      name = Name "name"
      (errs, _) = run $ runOutputList @Text $ renderNewTeamOwnerWelcomeEmail to tid tname name tpl branding
  assertNoErrors errs

testPasswordResetEmail :: (HasCallStack) => Map Text Text -> UserTemplates -> Http ()
testPasswordResetEmail branding templates = do
  let tpl = templates.passwordResetEmail
      to = fromJust $ emailAddressText "test@example.com"
      key = mkPasswordResetKey (Id UUID.nil)
      code = PasswordResetCode . encodeBase64Url $ "bar"
      (errs, _) = run $ runOutputList @Text $ renderPwResetMail to key code tpl branding
  assertNoErrors errs

testVerificationEmail :: (HasCallStack) => Map Text Text -> UserTemplates -> Http ()
testVerificationEmail branding templates = do
  let tpl = templates.verificationEmail
      to = fromJust $ emailAddressText "test@example.com"
      key = ActivationKey . Ascii.unsafeFromText $ "key"
      code = ActivationCode . Ascii.unsafeFromText $ "code"
      (errs, _) = run $ runOutputList @Text $ renderVerificationMail to key code tpl branding
  assertNoErrors errs

testTeamDeletionVerificationEmail :: (HasCallStack) => Map Text Text -> UserTemplates -> Http ()
testTeamDeletionVerificationEmail branding templates = do
  let tpl = templates.verificationTeamDeletionEmail
      to = fromJust $ emailAddressText "test@example.com"
      code = Value . unsafeRange . Ascii.unsafeFromText $ "code"
      (errs, _) = run $ runOutputList @Text $ renderSecondFactorVerificationEmail to code tpl branding
  assertNoErrors errs

testScimTokenVerificationEmail :: (HasCallStack) => Map Text Text -> UserTemplates -> Http ()
testScimTokenVerificationEmail branding templates = do
  let tpl = templates.verificationScimTokenEmail
      to = fromJust $ emailAddressText "test@example.com"
      code = Value . unsafeRange . Ascii.unsafeFromText $ "code"
      (errs, _) = run $ runOutputList @Text $ renderSecondFactorVerificationEmail to code tpl branding
  assertNoErrors errs

testLoginVerificationEmail :: (HasCallStack) => Map Text Text -> UserTemplates -> Http ()
testLoginVerificationEmail branding templates = do
  let tpl = templates.verificationLoginEmail
      to = fromJust $ emailAddressText "test@example.com"
      code = Value . unsafeRange . Ascii.unsafeFromText $ "code"
      (errs, _) = run $ runOutputList @Text $ renderSecondFactorVerificationEmail to code tpl branding
  assertNoErrors errs

testActivationEmail :: (HasCallStack) => Map Text Text -> UserTemplates -> Http ()
testActivationEmail branding templates = do
  let tpl = templates.activationEmail
      to = fromJust $ emailAddressText "test@example.com"
      name = Name "name"
      key = ActivationKey . Ascii.unsafeFromText $ "key"
      code = ActivationCode . Ascii.unsafeFromText $ "code"
      (errs, _) = run $ runOutputList @Text $ renderActivationMail to name key code tpl branding
  assertNoErrors errs

testActivationEmailUpdate :: (HasCallStack) => Map Text Text -> UserTemplates -> Http ()
testActivationEmailUpdate branding templates = do
  let tpl = templates.activationEmailUpdate
      to = fromJust $ emailAddressText "test@example.com"
      name = Name "name"
      key = ActivationKey . Ascii.unsafeFromText $ "key"
      code = ActivationCode . Ascii.unsafeFromText $ "code"
      (errs, _) = run $ runOutputList @Text $ renderActivationMail to name key code tpl branding
  assertNoErrors errs

testTeamActivationEmail :: (HasCallStack) => Map Text Text -> UserTemplates -> Http ()
testTeamActivationEmail branding templates = do
  let tpl = templates.teamActivationEmail
      to = fromJust $ emailAddressText "test@example.com"
      name = Name "name"
      teamName = "team-name"
      key = ActivationKey . Ascii.unsafeFromText $ "key"
      code = ActivationCode . Ascii.unsafeFromText $ "code"
      (errs, _) = run $ runOutputList @Text $ renderTeamActivationMail to name teamName key code tpl branding
  assertNoErrors errs

testNewClientEmail :: (HasCallStack) => Map Text Text -> Locale -> UserTemplates -> Http ()
testNewClientEmail branding loc templates = do
  let tpl = templates.newClientEmail
      to = fromJust $ emailAddressText "test@example.com"
      name = Name "name"
      client =
        Client
          { clientId = ClientId 1,
            clientType = PermanentClientType,
            clientTime = toUTCTimeMillis (UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)),
            clientClass = Nothing,
            clientLabel = Just "label",
            clientCookie = Nothing,
            clientModel = Just "model",
            clientCapabilities = ClientCapabilityList mempty,
            clientMLSPublicKeys = Map.empty,
            clientLastActive = Nothing
          }
      (errs, _) = run $ runOutputList @Text $ renderNewClientEmail to name loc client tpl branding
  assertNoErrors errs

testAccountDeletionEmail :: (HasCallStack) => Map Text Text -> UserTemplates -> Http ()
testAccountDeletionEmail branding templates = do
  let tpl = templates.deletionEmail
      to = fromJust $ emailAddressText "test@example.com"
      name = Name "name"
      key = Key . unsafeRange . Ascii.unsafeFromText $ "ABCDEFGHIJKLMNOPQRST"
      code = Value . unsafeRange . Ascii.unsafeFromText $ "code123"
      (errs, _) = run $ runOutputList @Text $ renderDeletionEmail to name key code tpl branding
  assertNoErrors errs

assertNoErrors :: [Text] -> Http ()
assertNoErrors errs =
  liftIO $
    assertBool ("The following variables were not replaced: " <> show (nub errs)) (null errs)
