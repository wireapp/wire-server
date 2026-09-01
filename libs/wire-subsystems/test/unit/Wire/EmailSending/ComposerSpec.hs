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

module Wire.EmailSending.ComposerSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Code qualified as Code
import Data.Id
import Data.Range (unsafeRange)
import Data.Text.Encoding (decodeUtf8)
import Data.UUID qualified as UUID
import Imports
import Network.Mail.Mime
import Polysemy
import Polysemy.Output
import Test.Hspec
import Test.QuickCheck (property, (===))
import Wire.API.BackgroundJobs.Email
import Wire.API.Jobs (EmailsJobPayload (SendEmail), SendEmailJobPayload (..))
import Wire.API.User
import Wire.API.User.Activation
import Wire.EmailSending.Composer
import Wire.EmailSubsystem.Interpreter (InvitationEmail (..), renderActivationMail, renderInvitationEmail, renderSecondFactorVerificationEmail)
import Wire.EmailSubsystem.Template (forLocale)
import Wire.EmailSubsystem.TemplateFixtures
import Wire.EmailSubsystem.Templates.Team (invitationEmail)
import Wire.EmailSubsystem.Templates.User qualified as U

spec :: Spec
spec = do
  templates <- runIO loadTestEmailTemplates
  describe "Wire.EmailSending.Composer" $ do
    describe "JSON roundtrip" $
      it "all variants roundtrip" $
        property $ \reqId req ->
          let job = SendEmail (SendEmailJobPayload reqId req)
           in Aeson.decode (Aeson.encode job) === Just job
    describe "composition" $ do
      it "activation request composes to the direct render" $ do
        let (errs, composed) =
              run . runOutputList @Text $
                composeEmail templates (ActivationEmail (MkActivationEmail testTo testName testKey testCode (Just defLocale)))
            tpl = activationEmailTpls templates
            (errs', direct) = run . runOutputList @Text $ renderActivationMail testTo testName testKey testCode tpl branding
        lookupHeader "Subject" composed `shouldBe` lookupHeader "Subject" direct
        composed.mailTo `shouldBe` direct.mailTo
        composed.mailFrom `shouldBe` direct.mailFrom
        length composed.mailParts `shouldBe` length direct.mailParts
        errs `shouldBe` errs'
        errs `shouldBe` []
      it "login verification composes to the direct render" $ do
        let req = LoginVerificationEmail (MkSecondFactorVerificationEmail testTo testCodeValue (Just defLocale))
            (errs, composed) = run . runOutputList @Text $ composeEmail templates req
            tpl = U.verificationLoginEmail . snd $ forLocale Nothing templates.userTemplates
            (errs', direct) = run . runOutputList @Text $ renderSecondFactorVerificationEmail testTo testCodeValue tpl branding
        composed.mailTo `shouldBe` direct.mailTo
        lookupHeader "Subject" composed `shouldBe` lookupHeader "Subject" direct
        errs `shouldBe` errs'
      it "scim token verification composes to the direct render" $ do
        let req = ScimTokenVerificationEmail (MkSecondFactorVerificationEmail testTo testCodeValue (Just defLocale))
            (errs, composed) = run . runOutputList @Text $ composeEmail templates req
            tpl = U.verificationScimTokenEmail . snd $ forLocale Nothing templates.userTemplates
            (errs', direct) = run . runOutputList @Text $ renderSecondFactorVerificationEmail testTo testCodeValue tpl branding
        composed.mailTo `shouldBe` direct.mailTo
        lookupHeader "Subject" composed `shouldBe` lookupHeader "Subject" direct
        errs `shouldBe` errs'
      it "team invitation composes to the direct render" $ do
        let req = TeamInvitationEmail (MkTeamInvitationEmail {to = testTo, teamId = testTeamId, inviter = testInviter, code = testCode2, locale = Nothing})
            (errs, composed) = run . runOutputList @Text $ composeEmail templates req
            tpl = invitationEmail . snd $ forLocale Nothing templates.teamTemplates
            (errs', direct) = run . runOutputList @Text $ renderInvitationEmail (InvitationEmail testTo testTeamId testCode2 testInviter) tpl branding
        composed.mailTo `shouldBe` (fst direct).mailTo
        lookupHeader "Subject" composed `shouldBe` lookupHeader "Subject" (fst direct)
        errs `shouldBe` errs'
      it "provider password reset composes with provider sender and purpose" $ do
        let (_, mail) =
              run . runOutputList @Text $
                composeEmail templates (ProviderPasswordResetEmail (MkProviderPasswordResetEmail testTo testCodeKey testCodeValue))
        mail.mailFrom.addressEmail `shouldBe` fromEmail emailSender
        lookupHeader "X-Zeta-Purpose" mail `shouldBe` Just "ProviderPasswordReset"
      it "enterprise audit composes with recipient, subject and purpose" $ do
        let (_, mail) =
              run . runOutputList @Text $
                composeEmail templates (EnterpriseAuditEmail (MkEnterpriseAuditEmail emailSender testTo "audit subject" "https://example.com/url" Nothing Nothing))
        mail.mailTo `shouldBe` [Address Nothing (fromEmail testTo)]
        lookupHeader "Subject" mail `shouldBe` Just "audit subject"
        lookupHeader "X-Zeta-Purpose" mail `shouldBe` Just "audit"

activationEmailTpls :: EmailTemplates -> U.ActivationEmailTemplate
activationEmailTpls templates =
  (.activationEmail) . snd $ forLocale Nothing templates.userTemplates

lookupHeader :: Text -> Mail -> Maybe Text
lookupHeader name mail =
  listToMaybe [v | (k, v) <- mail.mailHeaders, decodeUtf8 k == name]

testTo :: EmailAddress
testTo = fromJust $ emailAddressText "test@example.com"

testName :: Name
testName = Name "Test"

testKey :: ActivationKey
testKey = ActivationKey "testkey"

testCode :: ActivationCode
testCode = ActivationCode "testcode"

testTeamId :: TeamId
testTeamId = Id (fromJust (UUID.fromString "123e4567-e89b-12d3-a456-426614174000"))

testInviter :: EmailAddress
testInviter = fromJust $ emailAddressText "inviter@example.com"

testCode2 :: InvitationCode
testCode2 = InvitationCode "ZoMX0xs="

testCodeKey :: Code.Key
testCodeKey = Code.Key (unsafeRange "01234567890123456789")

testCodeValue :: Code.Value
testCodeValue = Code.Value (unsafeRange "testcode1")
