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
import Data.Map qualified as Map
import Data.Qualified
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
import Wire.API.Team.Member (TeamMember)
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
     GalleyAPIAccess,
     Now,
     State UTCTime,
     Random,
     State StdGen,
     Embed IO
   ]

interpretFeaturesConfigSubsystemPure :: InterpreterFor FeaturesConfigSubsystem r
interpretFeaturesConfigSubsystemPure = interpret $ \case
  GetDbFeatureRawInternal _tid -> pure def
  GetFeature _uid _tid -> pure def
  GetFeatureForTeam _tid -> pure def
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
    . runError @(Tagged 'InvalidOperation ())
    . interpretFeaturesConfigSubsystemPure
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
            { title = "Test Meeting",
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
        meeting.title `shouldBe` "Test Meeting"
        fetched `shouldBe` Just meeting

  it "fails to create a meeting if end time is before start time" $ do
    let now = UTCTime (fromGregorian 2026 1 1) 0
        gen = mkStdGen 42
        uid = Id $ read "00000000-0000-0000-0000-000000000001"
        zUser = toLocalUnsafe (Domain "wire.com") uid
        newMeeting =
          API.NewMeeting
            { title = "Invalid Meeting",
              startTime = addUTCTime 3600 now,
              endTime = addUTCTime 3500 now,
              recurrence = Nothing,
              invitedEmails = []
            }

    result <- runTestStack now gen Map.empty def $ createMeeting zUser newMeeting
    case result of
      Left (Tagged ()) -> pure ()
      _ -> fail $ "Expected InvalidOperation error, got: " <> show result
