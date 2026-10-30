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
import Data.Domain (Domain (..))
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Misc (HttpsUrl (..))
import Data.Qualified (Qualified (..))
import Data.Range
import Data.Text qualified as T
import Data.Text.Ascii (AsciiChars (validate), encodeBase64Url)
import Data.Text.Ascii qualified as Ascii
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Time.Zones.All (TZLabel (..))
import Data.UUID qualified as UUID
import Imports
import Network.Mail.Mime
import Polysemy
import Polysemy.Output
import Test.Hspec
import URI.ByteString (parseURI, strictURIParserOptions)
import Wire.API.Locale
import Wire.API.Meeting (Manager (..), Meeting (..), MeetingType (..), TimeZone (..))
import Wire.API.User (InvitationCode (InvitationCode, fromInvitationCode))
import Wire.API.User.Activation
import Wire.API.User.Client (Client (..), ClientCapabilityList (..), ClientType (..))
import Wire.API.User.EmailAddress
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.EmailSubsystem.Interpreter
import Wire.EmailSubsystem.Template
import Wire.EmailSubsystem.TemplateFixtures
import Wire.EmailSubsystem.Templates.Meeting
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
  meetingTemplates <- runIO loadTestMeetingTemplates
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
    describe "meeting" $
      for_ (byLocale meetingTemplates) $ \(loc, ts) ->
        describe (show loc) $ do
          it "invitation" $ testMeetingInvitationEmail ts
          it "invitation without join link" $ testMeetingInvitationEmailNoLink ts
          it "update" $ testMeetingUpdateEmail ts
          it "cancel" $ testMeetingCancelEmail ts
          it "ics escapes and folds text" $ testMeetingIcsEscapesAndFoldsText ts

meetingFixture :: Meeting
meetingFixture =
  Meeting
    { id = Qualified (Id (fromJust (UUID.fromString "123e4567-e89b-12d3-a456-426614174000"))) (Domain "example.com"),
      title = unsafeRange "Quarterly Sync",
      creator = Qualified (Id (fromJust (UUID.fromString "223e4567-e89b-12d3-a456-426614174001"))) (Domain "example.com"),
      startTime = UTCTime (fromGregorian 2026 10 12) (secondsToDiffTime (9 * 3600)),
      endTime = UTCTime (fromGregorian 2026 10 12) (secondsToDiffTime (10 * 3600)),
      tzid = TimeZone Europe__Berlin,
      mtype = Scheduled,
      managedBy = Wire,
      calendarId = Nothing,
      recurrence = Nothing,
      conversationId = Qualified (Id (fromJust (UUID.fromString "323e4567-e89b-12d3-a456-426614174002"))) (Domain "example.com"),
      invitedEmails = [fromJust (emailAddressText "invitee@example.com")],
      createdAt = UTCTime (fromGregorian 2026 10 1) 0,
      updatedAt = UTCTime (fromGregorian 2026 10 1) 0,
      link = Just joinUrlFixture,
      hasPassword = False
    }

joinUrlFixture :: HttpsUrl
joinUrlFixture = HttpsUrl (fromRight (error "invalid test join URL") (parseURI strictURIParserOptions "https://meet.example.com/join/"))

testMeetingInvitationEmail :: (HasCallStack) => MeetingTemplates -> Expectation
testMeetingInvitationEmail templates = do
  let tpl = templates.meetingInvitationEmail
      (errs, mail) = run $ runOutputList @Text $ renderMeetingInvitationEmail to hostName hostEmail meetingFixture tpl branding
      to = fromJust $ emailAddressText "invitee@example.com"
  assertNoErrors errs
  mail.mailFrom.addressEmail `shouldBe` fromEmail tpl.meetingEmailSender
  mail.mailTo `shouldBe` [Address Nothing "invitee@example.com"]
  lookup "Subject" mail.mailHeaders `shouldSatisfy` isSubjectOf "Quarterly Sync"
  lookup "X-Zeta-Purpose" mail.mailHeaders `shouldBe` Just "MeetingInvitation"
  plainTextOf mail `shouldContain` "https://meet.example.com/join/"
  assertIcsPartType mail "text/calendar; charset=utf-8; method=REQUEST"
  let ics = unfoldIcs (icsRaw mail)
  ics `shouldSatisfy` T.isInfixOf "METHOD:REQUEST"
  ics `shouldSatisfy` T.isInfixOf "UID:123e4567-e89b-12d3-a456-426614174000@example.com"
  ics `shouldSatisfy` T.isInfixOf "DTSTART:20261012T090000Z"
  ics `shouldSatisfy` T.isInfixOf "DTEND:20261012T100000Z"
  ics `shouldSatisfy` T.isInfixOf "DTSTAMP:20261001T000000Z"
  ics `shouldSatisfy` T.isInfixOf "SEQUENCE:1790812800"
  ics `shouldSatisfy` T.isInfixOf "SUMMARY:Quarterly Sync"
  ics `shouldSatisfy` T.isInfixOf "ORGANIZER;CN=\"Alice Host\":mailto:host@example.com"
  ics `shouldSatisfy` T.isInfixOf "mailto:invitee@example.com"
  ics `shouldSatisfy` T.isInfixOf "URL:https://meet.example.com/join/"
  ics `shouldSatisfy` T.isInfixOf "STATUS:CONFIRMED"

testMeetingInvitationEmailNoLink :: (HasCallStack) => MeetingTemplates -> Expectation
testMeetingInvitationEmailNoLink templates = do
  let tpl = templates.meetingInvitationEmail
      meeting = meetingFixture {link = Nothing}
      (errs, mail) = run $ runOutputList @Text $ renderMeetingInvitationEmail to hostName hostEmail meeting tpl branding
      to = fromJust $ emailAddressText "invitee@example.com"
  assertNoErrors errs
  lookup "Subject" mail.mailHeaders `shouldSatisfy` isSubjectOf "Quarterly Sync"
  unfoldIcs (icsRaw mail) `shouldSatisfy` not . T.isInfixOf "URL:"

testMeetingUpdateEmail :: (HasCallStack) => MeetingTemplates -> Expectation
testMeetingUpdateEmail templates = do
  let tpl = templates.meetingUpdateEmail
      summary = "Start time moved to 10:00"
      -- The update email carries a strictly larger SEQUENCE than the
      -- invitation (same UID) because updatedAt has moved on.
      meeting = meetingFixture {updatedAt = UTCTime (fromGregorian 2026 10 1) (secondsToDiffTime 3600)}
      (errs, mail) = run $ runOutputList @Text $ renderMeetingUpdateEmail to hostName hostEmail meeting summary tpl branding
      to = fromJust $ emailAddressText "invitee@example.com"
  assertNoErrors errs
  mail.mailFrom.addressEmail `shouldBe` fromEmail tpl.meetingEmailSender
  mail.mailTo `shouldBe` [Address Nothing "invitee@example.com"]
  lookup "Subject" mail.mailHeaders `shouldSatisfy` isSubjectOf "Quarterly Sync"
  lookup "X-Zeta-Purpose" mail.mailHeaders `shouldBe` Just "MeetingUpdate"
  plainTextOf mail `shouldContain` T.unpack summary
  plainTextOf mail `shouldContain` "Monday, 12 October 2026"
  let ics = unfoldIcs (icsRaw mail)
  ics `shouldSatisfy` T.isInfixOf "METHOD:REQUEST"
  ics `shouldSatisfy` T.isInfixOf "DTSTAMP:20261001T010000Z"
  ics `shouldSatisfy` T.isInfixOf "SEQUENCE:1790816400"

testMeetingCancelEmail :: (HasCallStack) => MeetingTemplates -> Expectation
testMeetingCancelEmail templates = do
  let tpl = templates.meetingCancelEmail
      (errs, mail) = run $ runOutputList @Text $ renderMeetingCancelEmail to hostName hostEmail meetingFixture tpl branding
      to = fromJust $ emailAddressText "invitee@example.com"
  assertNoErrors errs
  mail.mailFrom.addressEmail `shouldBe` fromEmail tpl.meetingEmailSender
  mail.mailTo `shouldBe` [Address Nothing "invitee@example.com"]
  lookup "Subject" mail.mailHeaders `shouldSatisfy` isSubjectOf "Quarterly Sync"
  lookup "X-Zeta-Purpose" mail.mailHeaders `shouldBe` Just "MeetingCancellation"
  plainTextOf mail `shouldContain` "Monday, 12 October 2026"
  let ics = unfoldIcs (icsRaw mail)
  ics `shouldSatisfy` T.isInfixOf "METHOD:CANCEL"
  ics `shouldSatisfy` T.isInfixOf "STATUS:CANCELLED"
  -- Cancellation increments the sequence (RFC 5546 §3.2.6) even without an
  -- intervening update: fixture updatedAt epoch 1790812800, so 1790812801.
  ics `shouldSatisfy` T.isInfixOf "SEQUENCE:1790812801"

-- The mail's text/calendar part (error if missing). The ICS lives in its own
-- multipart/mixed group, so search all groups, not just the first.
icsPart :: (HasCallStack) => Mail -> Part
icsPart mail =
  fromMaybe (error "No text/calendar part found") $
    find (\p -> "text/calendar" `T.isPrefixOf` p.partType) (concat mail.mailParts)

-- Assert the mail carries a text/calendar part with the expected content type
-- and attachment disposition, in its own multipart/mixed group (separate from
-- the text/html alternative group).
assertIcsPartType :: (HasCallStack) => Mail -> Text -> Expectation
assertIcsPartType mail partType = do
  let part = icsPart mail
  part.partType `shouldBe` partType
  part.partDisposition `shouldBe` AttachmentDisposition "meeting.ics"
  mail.mailParts `shouldSatisfy` (\groups -> length groups == 2 && [part] == last groups)

-- Raw ICS text of the mail's text/calendar part.
icsRaw :: (HasCallStack) => Mail -> Text
icsRaw mail = case (icsPart mail).partContent of
  PartContent content -> TL.toStrict (decodeUtf8 content)
  NestedParts ns -> error $ "Unexpected NestedParts: " ++ show ns

-- RFC 5545 unfolding: continuation lines begin with a single space.
unfoldIcs :: Text -> Text
unfoldIcs = T.replace "\r\n " ""

testMeetingIcsEscapesAndFoldsText :: (HasCallStack) => MeetingTemplates -> Expectation
testMeetingIcsEscapesAndFoldsText templates = do
  let tpl = templates.meetingInvitationEmail
      meeting =
        meetingFixture
          { title = unsafeRange ("Quarterly Sync, part 1; kickoff" <> T.replicate 150 "x")
          }
      crTitle = unsafeRange "Standup\rURL:http://evil.example\rEND:VEVENT\rBEGIN:VALARM"
      crHost = "Alice\rHost \"Injected\""
      to = fromJust $ emailAddressText "invitee@example.com"
      (errs, mail) = run $ runOutputList @Text $ renderMeetingInvitationEmail to hostName hostEmail meeting tpl branding
      (crErrs, crMail) =
        run . runOutputList @Text $
          renderMeetingInvitationEmail to crHost hostEmail (meetingFixture {title = crTitle}) tpl branding
  assertNoErrors errs
  assertNoErrors crErrs
  let ics = icsRaw mail
  ics `shouldSatisfy` T.isInfixOf "SUMMARY:Quarterly Sync\\, part 1\\; kickoff"
  ics `shouldSatisfy` T.isInfixOf "\r\n "
  -- CR-bearing user input must not inject raw carriage returns (which
  -- lenient ICS parsers treat as content-line terminators); canonical CRLF
  -- line endings are fine.
  let crIcs = icsRaw crMail
  T.replace "\r\n" "" crIcs `shouldNotSatisfy` T.any (== '\r')
  crIcs `shouldSatisfy` T.isInfixOf "SUMMARY:StandupURL:http://evil.exampleEND:VEVENTBEGIN:VALARM"
  crIcs `shouldSatisfy` T.isInfixOf "CN=\"AliceHost Injected\""

-- Subjects are localized; assert the rendered meeting title part (the
-- placeholder ${meeting_title} is locale-independent).
isSubjectOf :: Text -> Maybe Text -> Bool
isSubjectOf title = \case
  Just subj -> (": " <> title) `T.isSuffixOf` subj
  Nothing -> False

plainTextOf :: (HasCallStack) => Mail -> String
plainTextOf mail =
  let textPart =
        fromMaybe (error "No text part found") $
          find (\p -> p.partType == "text/plain; charset=utf-8") (head mail.mailParts)
   in case textPart.partContent of
        PartContent content -> TL.unpack (decodeUtf8 content)
        NestedParts ns -> error $ "Unexpected NestedParts: " ++ show ns

hostName :: Text
hostName = "Alice Host"

hostEmail :: EmailAddress
hostEmail = fromJust $ emailAddressText "host@example.com"

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
