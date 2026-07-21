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

module API.Meetings
  ( tests,
  )
where

import API.Util
import API.Util qualified as Util
import API.Util.TeamFeature as TeamFeatures
import Bilge hiding (method, timeout)
import Bilge.Assert
import Data.ByteString.Conversion (toByteString')
import Data.Qualified (qDomain, qUnqualified)
import Data.Range (unsafeRange)
import Data.Time (addUTCTime, getCurrentTime)
import Imports
import Network.Wai.Utilities.Error (label)
import Test.Tasty
import Test.Tasty.HUnit (assertBool)
import TestHelpers
import TestSetup
import Wire.API.Meeting qualified as API
import Wire.API.Team.Feature (Feature (..), FeatureStatus (..), MeetingsConfig (..))

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Meetings"
    [ test s "read endpoints treat a disabled feature as empty; write ops stay gated" testMeetingsReadsWhenDisabled
    ]

-- | Read endpoints (@GET /meetings/list@, @GET /meetings/{domain}/{id}@) must
-- treat a team with the @meetings@ feature disabled as "no meetings" rather
-- than returning @403 invalid-op@. Write endpoints (@POST /meetings@) keep the
-- hard gate and still return @403 invalid-op@.
testMeetingsReadsWhenDisabled :: TestM ()
testMeetingsReadsWhenDisabled = do
  (owner, teamId, []) <- Util.createBindingTeamWithNMembers 0
  now <- liftIO getCurrentTime
  let startTime = addUTCTime 600 now
      endTime = addUTCTime 600 startTime
      newMeeting =
        API.NewMeeting
          { API.startTime = startTime,
            API.endTime = endTime,
            API.recurrence = Nothing,
            API.title = unsafeRange "test meeting",
            API.invitedEmails = []
          }

  g <- viewGalley

  -- Default (enabled): create a meeting to use as the read target later.
  createResp <-
    post (g . path "/meetings" . zUser owner . Bilge.json newMeeting)
      <!! const 201 === statusCode
  let created = responseJsonUnsafe @API.MeetingWithConversation createResp
      qMeetingId = created.meeting.id

  -- Positive control while enabled: the created meeting is directly findable.
  get
    ( g
        . paths ["meetings", toByteString' (qDomain qMeetingId), toByteString' (qUnqualified qMeetingId)]
        . zUser owner
    )
    !!! const 200 === statusCode

  -- Disable the meetings feature for the team.
  TeamFeatures.putTeamFeature @MeetingsConfig owner teamId (Feature FeatureStatusDisabled MeetingsConfig)
    !!! const 200 === statusCode

  -- Read paths treat a disabled feature as "no meetings": list -> 200 [], get -> 404.
  get (g . path "/meetings/list" . zUser owner)
    !!! do
      const 200 === statusCode
      const (Right ([] :: [API.Meeting])) === responseJsonEither

  get
    ( g
        . paths ["meetings", toByteString' (qDomain qMeetingId), toByteString' (qUnqualified qMeetingId)]
        . zUser owner
    )
    !!! do
      const 404 === statusCode
      const (Just "meeting-not-found") === fmap label . responseJsonUnsafe

  -- Write paths keep the hard gate: create -> 403 invalid-op (unchanged behavior).
  post (g . path "/meetings" . zUser owner . Bilge.json newMeeting)
    !!! do
      const 403 === statusCode
      const (Just "invalid-op") === fmap label . responseJsonUnsafe

  -- Re-enable: the previously-created meeting is readable again.
  TeamFeatures.putTeamFeature @MeetingsConfig owner teamId (Feature FeatureStatusEnabled MeetingsConfig)
    !!! const 200 === statusCode

  listResp <-
    get (g . path "/meetings/list" . zUser owner)
      <!! const 200 === statusCode
  let meetings = responseJsonUnsafe @[API.Meeting] listResp
  liftIO $
    assertBool "created meeting is visible after re-enabling" $
      any (\m -> m.id == qMeetingId) meetings
