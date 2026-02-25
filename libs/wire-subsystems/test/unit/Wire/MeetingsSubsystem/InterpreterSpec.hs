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

module Wire.MeetingsSubsystem.InterpreterSpec (spec) where

import Data.Default (def)
import Data.Domain (Domain (..))
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Map qualified as Map
import Data.Qualified
import Data.Range (checked)
import Data.Set qualified as Set
import Data.Tagged
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.State
import System.Random (StdGen, mkStdGen)
import Test.Hspec
import Wire.API.Error (ErrorS)
import Wire.API.Error.Galley
import Wire.API.Meeting qualified as API
import Wire.API.Team.Feature
import Wire.API.Team.Member (TeamMember, mkTeamMember)
import Wire.API.Team.Permission (fullPermissions)
import Wire.ConversationSubsystem
import Wire.FeaturesConfigSubsystem
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.MeetingsStore qualified as Store
import Wire.MeetingsSubsystem
import Wire.MeetingsSubsystem.Interpreter
import Wire.MockInterpreters
import Wire.Sem.Now (Now)
import Wire.Sem.Random (Random)
import Wire.StoredConversation
import Wire.TeamSubsystem (TeamSubsystem)
import Wire.TeamSubsystem.GalleyAPI

type TestStack =
  '[ MeetingsSubsystem,
     Store.MeetingsStore,
     ConversationSubsystem,
     TeamSubsystem,
     FeaturesConfigSubsystem,
     ErrorS 'InvalidOperation,
     State (Map MeetingId Store.StoredMeeting),
     State (Map ConvId StoredConversation),
     State (Map ConvId (Set UserId)),
     GalleyAPIAccess,
     Now,
     State UTCTime,
     Random,
     State StdGen,
     Embed IO
   ]

interpretFeaturesConfigSubsystemPure :: AllTeamFeatures -> InterpreterFor FeaturesConfigSubsystem r
interpretFeaturesConfigSubsystemPure configs = interpret $ \case
  GetDbFeatureRawInternal _tid -> pure def
  GetFeature _uid _tid -> pure def
  GetFeatureForTeam _tid -> pure $ npProject configs
  GetFeatureForServer -> pure def
  GetFeatureForTeamUser _uid _mTid -> pure def
  GetAllTeamFeaturesForTeamMember _luid _tid -> pure def
  GetAllTeamFeaturesForTeam _tid -> pure def
  GetAllTeamFeaturesForServer -> pure def

runTestStack ::
  UTCTime ->
  StdGen ->
  Map TeamId [TeamMember] ->
  AllTeamFeatures ->
  Sem TestStack a ->
  IO (Either (Tagged 'InvalidOperation ()) a)
runTestStack now gen teams configs =
  runM
    . evalState gen
    . randomToStatefulStdGen
    . evalState now
    . interpretNowAsState
    . miniGalleyAPIAccess teams configs
    . evalState Map.empty
    . evalState Map.empty
    . evalState Map.empty
    . runError @(Tagged 'InvalidOperation ())
    . interpretFeaturesConfigSubsystemPure configs
    . interpretTeamSubsystemToGalleyAPI
    . inMemoryConversationSubsystemInterpreter
    . inMemoryMeetingsStoreInterpreter
    . interpretMeetingsSubsystem 3600

spec :: Spec
spec = describe "MeetingsSubsystem.Interpreter" $ do
  it "creates a meeting and can retrieve it" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Test Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ do
      (meeting, _conv) <- createMeeting zUser newMeeting
      fetched <- getMeeting zUser meeting.id
      pure (meeting, fetched)

    case result of
      Left err -> fail $ "Error: " <> show err
      Right (meeting, fetched) -> do
        meeting.title `shouldBe` fromJust (checked "Test Meeting")
        fetched `shouldBe` Just meeting

  it "fails to create a meeting if end time is before start time" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Invalid Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 3500 now,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ createMeeting zUser newMeeting
    void result `shouldBe` Left (Tagged ())

  describe "getMeeting access control" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid1 = Id $ read "00000000-0000-0000-0000-000000000001"
        uid2 = Id $ read "00000000-0000-0000-0000-000000000002"
        uid3 = Id $ read "00000000-0000-0000-0000-000000000003"
        zUser1 = toLocalUnsafe (Domain "wire.com") uid1
        zUser2 = toLocalUnsafe (Domain "wire.com") uid2
        zUser3 = toLocalUnsafe (Domain "wire.com") uid3
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember1 = mkTeamMember uid1 fullPermissions Nothing UserLegalHoldDisabled
        teamMember2 = mkTeamMember uid2 fullPermissions Nothing UserLegalHoldDisabled
        teamConfig = npUpdate @MeetingsPremiumConfig (LockableFeature FeatureStatusEnabled LockStatusUnlocked def) def

    it "returns Nothing for expired meeting" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Past Meeting",
                startTime = addUTCTime (-7200) now,
                endTime = addUTCTime (-5000) now,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        (meeting, _conv) <- createMeeting zUser1 newMeeting
        getMeeting zUser1 meeting.id

      result `shouldBe` Right Nothing

    it "returns meeting for creator" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Creator Access Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen Map.empty teamConfig $ do
        (meeting, _conv) <- createMeeting zUser1 newMeeting
        (meeting,) <$> getMeeting zUser1 meeting.id

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (meeting, Just m) -> m.id `shouldBe` meeting.id
        Right (_, Nothing) -> fail "Expected Just meeting for creator"

    it "returns meeting for conversation member" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Member Access Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1, teamMember2]) teamConfig $ do
        (meeting, conv) <- createMeeting zUser1 newMeeting
        members <- gets (Map.lookup conv.id_)
        let updatedMembers = maybe (Set.singleton uid2) (Set.insert uid2) members
        modify (Map.insert conv.id_ updatedMembers)
        (meeting,) <$> getMeeting zUser2 meeting.id

      case result of
        Left err -> fail $ "Error: " <> show err
        Right (meeting, Just m) -> m.id `shouldBe` meeting.id
        Right (_, Nothing) -> fail "Expected Just meeting for conversation member"

    it "returns Nothing for unauthorized user" $ do
      let newMeeting =
            API.NewMeeting
              { title = fromJust $ checked "Unauthorized Meeting",
                startTime = addUTCTime 3600 now,
                endTime = addUTCTime 7200 now,
                recurrence = Nothing,
                invitedEmails = []
              }

      result <- runTestStack now gen (Map.singleton teamId [teamMember1]) teamConfig $ do
        (meeting, _conv) <- createMeeting zUser1 newMeeting
        getMeeting zUser3 meeting.id

      result `shouldBe` Right Nothing

  it "creates trial meeting for personal user" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Personal Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ do
      (meeting, _conv) <- createMeeting zUser newMeeting
      pure meeting

    fmap (.trial) result `shouldBe` Right True

  it "creates meeting with trial flag when premium is enabled for team" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember = mkTeamMember uid fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate
            @MeetingsPremiumConfig
            (LockableFeature FeatureStatusEnabled LockStatusUnlocked def)
            def
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Team Premium Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen (Map.singleton teamId [teamMember]) teamConfig $ do
      (meeting, _conv) <- createMeeting zUser newMeeting
      pure meeting

    fmap (.trial) result `shouldBe` Right False

  it "creates meeting without trial flag when premium is disabled for team" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        teamId = Id $ read "00000000-0000-0000-0000-000000000100"
        teamMember = mkTeamMember uid fullPermissions Nothing UserLegalHoldDisabled
        teamConfig =
          npUpdate
            @MeetingsPremiumConfig
            (LockableFeature FeatureStatusDisabled LockStatusUnlocked def)
            def
        newMeeting =
          API.NewMeeting
            { title = fromJust $ checked "Team Free Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 7200 now,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen (Map.singleton teamId [teamMember]) teamConfig $ do
      (meeting, _conv) <- createMeeting zUser newMeeting
      pure meeting

    fmap (.trial) result `shouldBe` Right True
