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

module Wire.EmailSubsystem.TemplateSpec (spec) where

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
import Wire.EmailSubsystem.Interpreter
import Wire.EmailSubsystem.Template
import Wire.EmailSubsystem.TemplateFixtures
import Wire.EmailSubsystem.Templates.Team
import Wire.EmailSubsystem.Templates.User

-- | Insert the default locale into the map of other locales, giving the full
-- set of locales that ship templates. Each is exercised below.
byLocale :: Localised a -> [(Locale, a)]
byLocale l = Map.assocs $ uncurry Map.insert l.locDefault l.locOther

spec :: Spec
spec = do
  teamTemplates <- runIO loadTestTeamTemplates
  userTemplates <- runIO loadTestUserTemplates
  describe "email templates" $ do
    describe "team" $
      for_ (byLocale teamTemplates) $ \(loc, ts) ->
        describe (show loc) $ do
          it "team invitation" $ testTeamInvitationEmail ts
          it "team invitation existing user" $ testTeamInvitationEmailExistingUser ts
          it "member welcome" $ testMemberWelcomeEmail ts
          it "new team owner welcome" $ testNewTeamOwnerWelcomeEmail ts
    describe "user" $
      for_ (byLocale userTemplates) $ \(loc, ts) ->
        describe (show loc) $ do
          it "password reset email" $ testPasswordResetEmail ts
          it "verification email" $ testVerificationEmail ts
          it "team deletion verification email" $ testTeamDeletionVerificationEmail ts
          it "scim token verification email" $ testScimTokenVerificationEmail ts
          it "login verification email" $ testLoginVerificationEmail ts
          it "new client email" $ testNewClientEmail loc ts
          it "account deletion email" $ testAccountDeletionEmail ts
          it "activation email" $ testActivationEmail ts
          it "activation email update" $ testActivationEmailUpdate ts
          it "team activation email" $ testTeamActivationEmail ts

testTeamInvitationEmailExistingUser :: (HasCallStack) => TeamTemplates -> Expectation
testTeamInvitationEmailExistingUser templates = do
  let tpl = templates.existingUserInvitationEmail
      (errs, (mail, url)) = run $ runOutputList @Text $ renderInvitationEmail input tpl branding
      input =
        InvitationEmail
          { invTo = fromJust $ emailAddressText "test@example.com",
            invTeamId = Id (fromJust $ UUID.fromString "123e4567-e89b-12d3-a456-426614174000"),
            invInvCode = InvitationCode {fromInvitationCode = fromRight undefined (validate "ZoMX0xs=")},
            invInviter = fromJust $ emailAddressText "inviter@example.com"
          }
  mail.mailFrom.addressEmail `shouldBe` fromEmail tpl.invitationEmailSender
  url `shouldBe` "https://example.com/accept-invitation/?team-code=ZoMX0xs="
  assertNoErrors errs

testTeamInvitationEmail :: (HasCallStack) => TeamTemplates -> Expectation
testTeamInvitationEmail templates = do
  let tpl = templates.invitationEmail
      (errs, (mail, url)) = run $ runOutputList @Text $ renderInvitationEmail input tpl branding
      input =
        InvitationEmail
          { invTo = fromJust $ emailAddressText "test@example.com",
            invTeamId = Id (fromJust $ UUID.fromString "123e4567-e89b-12d3-a456-426614174000"),
            invInvCode = InvitationCode {fromInvitationCode = fromRight undefined (validate "ZoMX0xs=")},
            invInviter = fromJust $ emailAddressText "inviter@example.com"
          }
  mail.mailFrom.addressEmail `shouldBe` fromEmail tpl.invitationEmailSender
  url `shouldBe` "https://example.com/join/?team-code=ZoMX0xs="
  assertNoErrors errs

testMemberWelcomeEmail :: (HasCallStack) => TeamTemplates -> Expectation
testMemberWelcomeEmail templates = do
  let tpl = templates.memberWelcomeEmail
      to = fromJust $ emailAddressText "test@example.com"
      tid = Id (fromJust $ UUID.fromString "123e4567-e89b-12d3-a456-426614174000")
      tname = "funky team"
      (errs, _) = run $ runOutputList @Text $ renderMemberWelcomeMail to tid tname tpl branding
  assertNoErrors errs

testNewTeamOwnerWelcomeEmail :: (HasCallStack) => TeamTemplates -> Expectation
testNewTeamOwnerWelcomeEmail templates = do
  let tpl = templates.newTeamOwnerWelcomeEmail
      to = fromJust $ emailAddressText "test@example.com"
      tid = Id (fromJust $ UUID.fromString "123e4567-e89b-12d3-a456-426614174000")
      tname = "funky team"
      name = Name "name"
      (errs, _) = run $ runOutputList @Text $ renderNewTeamOwnerWelcomeEmail to tid tname name tpl branding
  assertNoErrors errs

testPasswordResetEmail :: (HasCallStack) => UserTemplates -> Expectation
testPasswordResetEmail templates = do
  let tpl = templates.passwordResetEmail
      to = fromJust $ emailAddressText "test@example.com"
      key = mkPasswordResetKey (Id UUID.nil)
      code = PasswordResetCode . encodeBase64Url $ "bar"
      (errs, _) = run $ runOutputList @Text $ renderPwResetMail to key code tpl branding
  assertNoErrors errs

testVerificationEmail :: (HasCallStack) => UserTemplates -> Expectation
testVerificationEmail templates = do
  let tpl = templates.verificationEmail
      to = fromJust $ emailAddressText "test@example.com"
      key = ActivationKey . Ascii.unsafeFromText $ "key"
      code = ActivationCode . Ascii.unsafeFromText $ "code"
      (errs, _) = run $ runOutputList @Text $ renderVerificationMail to key code tpl branding
  assertNoErrors errs

testTeamDeletionVerificationEmail :: (HasCallStack) => UserTemplates -> Expectation
testTeamDeletionVerificationEmail templates = do
  let tpl = templates.verificationTeamDeletionEmail
      to = fromJust $ emailAddressText "test@example.com"
      code = Value . unsafeRange . Ascii.unsafeFromText $ "code"
      (errs, _) = run $ runOutputList @Text $ renderSecondFactorVerificationEmail to code tpl branding
  assertNoErrors errs

testScimTokenVerificationEmail :: (HasCallStack) => UserTemplates -> Expectation
testScimTokenVerificationEmail templates = do
  let tpl = templates.verificationScimTokenEmail
      to = fromJust $ emailAddressText "test@example.com"
      code = Value . unsafeRange . Ascii.unsafeFromText $ "code"
      (errs, _) = run $ runOutputList @Text $ renderSecondFactorVerificationEmail to code tpl branding
  assertNoErrors errs

testLoginVerificationEmail :: (HasCallStack) => UserTemplates -> Expectation
testLoginVerificationEmail templates = do
  let tpl = templates.verificationLoginEmail
      to = fromJust $ emailAddressText "test@example.com"
      code = Value . unsafeRange . Ascii.unsafeFromText $ "code"
      (errs, _) = run $ runOutputList @Text $ renderSecondFactorVerificationEmail to code tpl branding
  assertNoErrors errs

testActivationEmail :: (HasCallStack) => UserTemplates -> Expectation
testActivationEmail templates = do
  let tpl = templates.activationEmail
      to = fromJust $ emailAddressText "test@example.com"
      name = Name "name"
      key = ActivationKey . Ascii.unsafeFromText $ "key"
      code = ActivationCode . Ascii.unsafeFromText $ "code"
      (errs, _) = run $ runOutputList @Text $ renderActivationMail to name key code tpl branding
  assertNoErrors errs

testActivationEmailUpdate :: (HasCallStack) => UserTemplates -> Expectation
testActivationEmailUpdate templates = do
  let tpl = templates.activationEmailUpdate
      to = fromJust $ emailAddressText "test@example.com"
      name = Name "name"
      key = ActivationKey . Ascii.unsafeFromText $ "key"
      code = ActivationCode . Ascii.unsafeFromText $ "code"
      (errs, _) = run $ runOutputList @Text $ renderActivationMail to name key code tpl branding
  assertNoErrors errs

testTeamActivationEmail :: (HasCallStack) => UserTemplates -> Expectation
testTeamActivationEmail templates = do
  let tpl = templates.teamActivationEmail
      to = fromJust $ emailAddressText "test@example.com"
      name = Name "name"
      teamName = "team-name"
      key = ActivationKey . Ascii.unsafeFromText $ "key"
      code = ActivationCode . Ascii.unsafeFromText $ "code"
      (errs, _) = run $ runOutputList @Text $ renderTeamActivationMail to name teamName key code tpl branding
  assertNoErrors errs

testNewClientEmail :: (HasCallStack) => Locale -> UserTemplates -> Expectation
testNewClientEmail loc templates = do
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

testAccountDeletionEmail :: (HasCallStack) => UserTemplates -> Expectation
testAccountDeletionEmail templates = do
  let tpl = templates.deletionEmail
      to = fromJust $ emailAddressText "test@example.com"
      name = Name "name"
      key = Key . unsafeRange . Ascii.unsafeFromText $ "ABCDEFGHIJKLMNOPQRST"
      code = Value . unsafeRange . Ascii.unsafeFromText $ "code123"
      (errs, _) = run $ runOutputList @Text $ renderDeletionEmail to name key code tpl branding
  assertNoErrors errs

assertNoErrors :: (HasCallStack) => [Text] -> Expectation
assertNoErrors errs =
  unless (null errs) $
    expectationFailure ("The following variables were not replaced: " <> show (nub errs))
