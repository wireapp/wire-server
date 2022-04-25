-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module API.MessageTimer
  ( tests,
  )
where

import API.Util
import Bilge hiding (timeout)
import Bilge.Assert
import Control.Lens (view)
import Data.Aeson (eitherDecode)
import Data.Domain
import Data.Id
import qualified Data.LegalHold as LH
import Data.List1
import qualified Data.List1 as List1
import Data.Misc
import Data.Qualified
import Data.Singletons
import Federator.MockServer (FederatedRequest (..))
import Galley.Types
import Galley.Types.Conversations.Roles
import qualified Galley.Types.Teams as Teams
import Imports hiding (head)
import Network.Wai.Utilities.Error
import Test.Tasty
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation.Action
import Wire.API.Event.Conversation
import qualified Wire.API.Federation.API.Galley as F
import Wire.API.Federation.Component
import Wire.API.Internal.Notification (Notification (..))
import qualified Wire.API.Team.Member as Member

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Per-conversation message timer"
    [ testGroup
        "timer can be set at creation time"
        [ test s "1 second" (messageTimerInit timer1sec),
          test s "nothing" (messageTimerInit Nothing)
        ],
      test s "timer can be changed" messageTimerChange,
      test s "timer can be changed with the qualified endpoint" messageTimerChangeQualified,
      test s "timer changes are propagated to remote users" messageTimerChangeWithRemotes,
      test s "timer can't be set by conv member without allowed action" messageTimerChangeWithoutAllowedAction,
      test s "timer can't be set in 1:1 conversations" messageTimerChangeO2O,
      test s "setting the timer generates an event" messageTimerEvent
    ]

messageTimerInit ::
  -- | Timer value
  Maybe Milliseconds ->
  TestM ()
messageTimerInit mtimer = do
  -- Create a conversation with a timer
  [alice, bob, jane] <- randomUsers 3
  qAlice <- Qualified <$> pure alice <*> viewFederationDomain
  connectUsers alice (list1 bob [jane])
  rsp <-
    postConv alice [bob, jane] Nothing [] Nothing mtimer
      <!! const 201 === statusCode
  cid <- assertConv rsp RegularConv alice qAlice [bob, jane] Nothing mtimer
  -- Check that the timer is indeed what it should be
  getConv jane cid
    !!! const mtimer === (cnvMessageTimer <=< responseJsonUnsafe)

messageTimerChange :: TestM ()
messageTimerChange = do
  -- Create a conversation without a timer
  [alice, bob, jane] <- randomUsers 3
  qAlice <- Qualified <$> pure alice <*> viewFederationDomain
  connectUsers alice (list1 bob [jane])
  rsp <-
    postConv alice [bob, jane] Nothing [] Nothing Nothing
      <!! const 201 === statusCode
  cid <- assertConv rsp RegularConv alice qAlice [bob, jane] Nothing Nothing
  -- Set timer to null and observe 204
  putMessageTimerUpdate alice cid (ConversationMessageTimerUpdate Nothing)
    !!! const 204 === statusCode
  -- Set timer to 1 second
  putMessageTimerUpdate alice cid (ConversationMessageTimerUpdate timer1sec)
    !!! const 200 === statusCode
  getConv jane cid
    !!! const timer1sec === (cnvMessageTimer <=< responseJsonUnsafe)
  -- Set timer to null
  putMessageTimerUpdate bob cid (ConversationMessageTimerUpdate Nothing)
    !!! const 200 === statusCode
  getConv jane cid
    !!! const Nothing === (cnvMessageTimer <=< responseJsonUnsafe)
  -- Set timer to 1 year
  putMessageTimerUpdate bob cid (ConversationMessageTimerUpdate timer1year)
    !!! const 200 === statusCode
  getConv jane cid
    !!! const timer1year === (cnvMessageTimer <=< responseJsonUnsafe)

messageTimerChangeQualified :: TestM ()
messageTimerChangeQualified = do
  localDomain <- viewFederationDomain
  -- Create a conversation without a timer
  [alice, bob, jane] <- randomUsers 3
  qAlice <- Qualified <$> pure alice <*> viewFederationDomain
  connectUsers alice (list1 bob [jane])
  rsp <-
    postConv alice [bob, jane] Nothing [] Nothing Nothing
      <!! const 201 === statusCode
  cid <- assertConv rsp RegularConv alice qAlice [bob, jane] Nothing Nothing
  let qcid = Qualified cid localDomain
  -- Set timer to null and observe 204
  putMessageTimerUpdateQualified alice qcid (ConversationMessageTimerUpdate Nothing)
    !!! const 204 === statusCode
  -- Set timer to 1 second
  putMessageTimerUpdateQualified alice qcid (ConversationMessageTimerUpdate timer1sec)
    !!! const 200 === statusCode
  getConv jane cid
    !!! const timer1sec === (cnvMessageTimer <=< responseJsonUnsafe)
  -- Set timer to null
  putMessageTimerUpdateQualified bob qcid (ConversationMessageTimerUpdate Nothing)
    !!! const 200 === statusCode
  getConv jane cid
    !!! const Nothing === (cnvMessageTimer <=< responseJsonUnsafe)
  -- Set timer to 1 year
  putMessageTimerUpdateQualified bob qcid (ConversationMessageTimerUpdate timer1year)
    !!! const 200 === statusCode
  getConv jane cid
    !!! const timer1year === (cnvMessageTimer <=< responseJsonUnsafe)

messageTimerChangeWithRemotes :: TestM ()
messageTimerChangeWithRemotes = do
  c <- view tsCannon
  let remoteDomain = Domain "alice.example.com"
  qalice <- Qualified <$> randomId <*> pure remoteDomain
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  connectWithRemoteUser bob qalice

  resp <-
    postConvWithRemoteUsers
      bob
      defNewProteusConv {newConvQualifiedUsers = [qalice]}
  let qconv = decodeQualifiedConvId resp

  WS.bracketR c bob $ \wsB -> do
    (_, requests) <-
      withTempMockFederator (const ()) $
        putMessageTimerUpdateQualified bob qconv (ConversationMessageTimerUpdate timer1sec)
          !!! const 200 === statusCode

    req <- assertOne requests
    liftIO $ do
      frTargetDomain req @?= remoteDomain
      frComponent req @?= Galley
      frRPC req @?= "on-conversation-updated"
      Right cu <- pure . eitherDecode . frBody $ req
      F.cuConvId cu @?= qUnqualified qconv
      F.cuAction cu
        @?= SomeConversationAction (sing @'ConversationMessageTimerUpdateTag) (ConversationMessageTimerUpdate timer1sec)

    void . liftIO . WS.assertMatch (5 # Second) wsB $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= ConvMessageTimerUpdate
      evtFrom e @?= qbob
      evtData e @?= EdConvMessageTimerUpdate (ConversationMessageTimerUpdate timer1sec)

messageTimerChangeWithoutAllowedAction :: TestM ()
messageTimerChangeWithoutAllowedAction = do
  -- Create a team and a guest user
  [owner, member, guest] <- randomUsers 3
  connectUsers owner (list1 member [guest])
  tid <- createNonBindingTeam "team" owner [Member.mkTeamMember member Teams.fullPermissions Nothing LH.defUserLegalHoldStatus]
  -- Create a conversation
  cid <- createTeamConvWithRole owner tid [member, guest] Nothing Nothing Nothing roleNameWireMember
  -- Try to change the timer (as a non admin, guest user) and observe failure
  putMessageTimerUpdate guest cid (ConversationMessageTimerUpdate timer1sec) !!! do
    const 403 === statusCode
    const "action-denied" === (label . responseJsonUnsafeWithMsg "error label")
  getConv guest cid
    !!! const Nothing === (cnvMessageTimer <=< responseJsonUnsafe)
  -- Try to change the timer (as a non admin, team member) and observe failure too
  putMessageTimerUpdate member cid (ConversationMessageTimerUpdate timer1sec) !!! do
    const 403 === statusCode
    const "action-denied" === (label . responseJsonUnsafeWithMsg "error label")
  -- Finally try to change the timer (as an admin) and observe success
  putMessageTimerUpdate owner cid (ConversationMessageTimerUpdate timer1sec) !!! do
    const 200 === statusCode
  getConv guest cid
    !!! const timer1sec === (cnvMessageTimer <=< responseJsonUnsafe)

messageTimerChangeO2O :: TestM ()
messageTimerChangeO2O = do
  -- Create a 1:1 conversation
  [alice, bob] <- randomUsers 2
  qAlice <- Qualified <$> pure alice <*> viewFederationDomain
  connectUsers alice (singleton bob)
  rsp <-
    postO2OConv alice bob Nothing
      <!! const 200 === statusCode
  cid <- assertConv rsp One2OneConv alice qAlice [bob] Nothing Nothing
  -- Try to change the timer and observe failure
  putMessageTimerUpdate alice cid (ConversationMessageTimerUpdate timer1sec) !!! do
    const 403 === statusCode
    const "invalid-op" === (label . responseJsonUnsafeWithMsg "error label")
  getConv alice cid
    !!! const Nothing === (cnvMessageTimer <=< responseJsonMaybe)

messageTimerEvent :: TestM ()
messageTimerEvent = do
  localDomain <- viewFederationDomain
  ca <- view tsCannon
  -- Create a conversation
  [alice, bob] <- randomUsers 2
  qAlice <- Qualified <$> pure alice <*> viewFederationDomain
  connectUsers alice (singleton bob)
  rsp <-
    postConv alice [bob] Nothing [] Nothing Nothing
      <!! const 201 === statusCode
  cid <- assertConv rsp RegularConv alice qAlice [bob] Nothing Nothing
  -- Set timer to 1 second and check that all participants got the event
  WS.bracketR2 ca alice bob $ \(wsA, wsB) -> do
    let update = ConversationMessageTimerUpdate timer1sec
        qcid = Qualified cid localDomain
        qalice = Qualified alice localDomain
    putMessageTimerUpdate alice cid update
      !!! const 200 === statusCode
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB] $
        wsAssertConvMessageTimerUpdate qcid qalice update

----------------------------------------------------------------------------
-- Utilities

timer1sec :: Maybe Milliseconds
timer1sec = Just 1000

timer1year :: Maybe Milliseconds
timer1year = Just (365 * 86400 * 1000)
