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
-- with this program.  If not, see <https://www.gnu.org/licenses/>.

module Wire.EmailSubsystem.InterpreterSpec (spec) where

import Control.Exception (ErrorCall (..), throwIO, try)
import Data.Domain (Domain (..))
import Data.Id (Id (..))
import Data.Map qualified as Map
import Data.Misc (HttpsUrl (..))
import Data.Qualified (Qualified (..))
import Data.Range (unsafeRange)
import Data.Text.Ascii qualified as Ascii
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Time.Zones.All (TZLabel (..))
import Data.UUID qualified as UUID
import Imports
import Polysemy
import System.IO.Error (userError)
import Test.Hspec
import URI.ByteString (parseURI, strictURIParserOptions)
import Wire.API.Meeting (Meeting (..), MeetingType (..), TimeZone (..))
import Wire.API.User.Activation
import Wire.API.User.EmailAddress
import Wire.EmailSending (EmailSending (SendMail))
import Wire.EmailSubsystem (sendMeetingInvitationEmail, sendVerificationMail)
import Wire.EmailSubsystem.Interpreter
import Wire.EmailSubsystem.Template (Branding)
import Wire.EmailSubsystem.TemplateFixtures (loadTestEmailTemplates)
import Wire.Sem.Logger.TinyLog (discardTinyLogs)

spec :: Spec
spec = do
  emailTemplates <- runIO loadTestEmailTemplates
  describe "email subsystem interpreter" $
    describe "meeting emails" $ do
      it "keeps sending after a transport failure for an earlier recipient" $ do
        attemptsRef <- newIORef (0 :: Int)
        result <-
          try @SomeException
            $ runFinal @IO
              . embedToFinal @IO
              . discardTinyLogs
              . mockEmailSending attemptsRef
              . emailSubsystemInterpreter emailTemplates branding Nothing
            $ do
              sendMeetingInvitationEmail to hostName hostEmail meetingFixture Nothing
              sendMeetingInvitationEmail to2 hostName hostEmail meetingFixture Nothing
              liftIO (readIORef attemptsRef)
        case result of
          Left e -> expectationFailure ("unexpected exception: " <> displayException e)
          Right attempts -> attempts `shouldBe` 2
      it "propagates send failures of non-meeting emails" $ do
        -- The best-effort catch is meeting-only; every other email kind
        -- still lets transport errors escape to the caller.
        result <-
          try @SomeException
            $ runFinal @IO
              . embedToFinal @IO
              . discardTinyLogs
              . mockFailingEmailSending
              . emailSubsystemInterpreter emailTemplates branding Nothing
            $ sendVerificationMail to activationKeyFixture activationCodeFixture Nothing
        case result of
          Left _ -> pure ()
          Right () -> expectationFailure "expected the transport error to escape"

branding :: Branding
branding = Map.empty

hostName :: Text
hostName = "Alice Host"

hostEmail :: EmailAddress
hostEmail = fromJust $ emailAddressText "host@example.com"

to :: EmailAddress
to = fromJust $ emailAddressText "invitee@example.com"

to2 :: EmailAddress
to2 = fromJust $ emailAddressText "other@example.com"

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
      recurrence = Nothing,
      conversationId = Qualified (Id (fromJust (UUID.fromString "323e4567-e89b-12d3-a456-426614174002"))) (Domain "example.com"),
      invitedEmails = [],
      createdAt = UTCTime (fromGregorian 2026 10 1) 0,
      updatedAt = UTCTime (fromGregorian 2026 10 1) 0,
      link = joinUrlFixture,
      hasPassword = False
    }

joinUrlFixture :: HttpsUrl
joinUrlFixture = HttpsUrl (fromRight (error "invalid test join URL") (parseURI strictURIParserOptions "https://meet.example.com/join/"))

activationKeyFixture :: ActivationKey
activationKeyFixture = ActivationKey . Ascii.unsafeFromText $ "key"

activationCodeFixture :: ActivationCode
activationCodeFixture = ActivationCode . Ascii.unsafeFromText $ "code"

-- | Throws on the first 'SendMail', succeeds afterwards; counts attempts in
-- the given ref (a shared mutable count).
mockEmailSending :: (Member (Embed IO) r) => IORef Int -> InterpreterFor EmailSending r
mockEmailSending attemptsRef = interpret \case
  SendMail _mail -> do
    attempts <- liftIO (atomicModifyIORef' attemptsRef (\n -> (n + 1, n)))
    when (attempts == 0) . embed $ throwIO (userError "mock: smtp down")

-- | Always throws.
mockFailingEmailSending :: (Member (Embed IO) r) => InterpreterFor EmailSending r
mockFailingEmailSending = interpret \case
  SendMail _mail -> embed $ throwIO (ErrorCall "mock: smtp down")
