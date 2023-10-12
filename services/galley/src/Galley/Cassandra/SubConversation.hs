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

module Galley.Cassandra.SubConversation
  ( interpretSubConversationStoreToCassandra,
  )
where

import Cassandra
import Cassandra.Util
import Control.Error.Util
import Control.Monad.Trans.Maybe
import Data.Id
import Data.Map qualified as Map
import Data.Time.Clock
import Galley.API.MLS.Types
import Galley.Cassandra.Conversation.MLS
import Galley.Cassandra.Queries qualified as Cql
import Galley.Cassandra.Store (embedClient)
import Galley.Effects.SubConversationStore (SubConversationStore (..))
import Imports hiding (cs)
import Polysemy
import Polysemy.Input
import Wire.API.Conversation.Protocol
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Group
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.SubConversation

selectSubConversation :: ConvId -> SubConvId -> Client (Maybe SubConversation)
selectSubConversation convId subConvId = runMaybeT $ do
  (mSuite, mEpoch, mEpochWritetime, mGroupId) <-
    MaybeT $
      retry x5 (query1 Cql.selectSubConversation (params LocalQuorum (convId, subConvId)))
  suite <- hoistMaybe mSuite
  epoch <- hoistMaybe mEpoch
  epochWritetime <- hoistMaybe mEpochWritetime
  groupId <- hoistMaybe mGroupId
  (cm, im) <- lift $ lookupMLSClientLeafIndices groupId
  pure $
    SubConversation
      { scParentConvId = convId,
        scSubConvId = subConvId,
        scMLSData =
          ConversationMLSData
            { cnvmlsGroupId = groupId,
              cnvmlsEpoch = epoch,
              cnvmlsEpochTimestamp = epochTimestamp epoch epochWritetime,
              cnvmlsCipherSuite = suite
            },
        scMembers = cm,
        scIndexMap = im
      }

insertSubConversation ::
  ConvId ->
  SubConvId ->
  CipherSuiteTag ->
  GroupId ->
  Client SubConversation
insertSubConversation convId subConvId suite groupId = do
  retry
    x5
    ( write
        Cql.insertSubConversation
        ( params
            LocalQuorum
            (convId, subConvId, suite, Epoch 0, groupId, Nothing)
        )
    )
  pure (newSubConversation convId subConvId suite groupId)

updateSubConvGroupInfo :: ConvId -> SubConvId -> Maybe GroupInfoData -> Client ()
updateSubConvGroupInfo convId subConvId mGroupInfo =
  retry x5 (write Cql.updateSubConvGroupInfo (params LocalQuorum (convId, subConvId, mGroupInfo)))

selectSubConvGroupInfo :: ConvId -> SubConvId -> Client (Maybe GroupInfoData)
selectSubConvGroupInfo convId subConvId =
  (runIdentity =<<) <$> retry x5 (query1 Cql.selectSubConvGroupInfo (params LocalQuorum (convId, subConvId)))

selectSubConvEpoch :: ConvId -> SubConvId -> Client (Maybe Epoch)
selectSubConvEpoch convId subConvId =
  (runIdentity =<<) <$> retry x5 (query1 Cql.selectSubConvEpoch (params LocalQuorum (convId, subConvId)))

setEpochForSubConversation :: ConvId -> SubConvId -> Epoch -> Client ()
setEpochForSubConversation cid sconv epoch =
  retry x5 (write Cql.insertEpochForSubConversation (params LocalQuorum (epoch, cid, sconv)))

setCipherSuiteForSubConversation :: ConvId -> SubConvId -> CipherSuiteTag -> Client ()
setCipherSuiteForSubConversation cid sconv cs =
  retry x5 (write Cql.insertCipherSuiteForSubConversation (params LocalQuorum (cs, cid, sconv)))

deleteSubConversation :: ConvId -> SubConvId -> Client ()
deleteSubConversation cid sconv =
  retry x5 $ write Cql.deleteSubConversation (params LocalQuorum (cid, sconv))

listSubConversations :: ConvId -> Client (Map SubConvId ConversationMLSData)
listSubConversations cid = do
  subs <- retry x1 (query Cql.listSubConversations (params LocalQuorum (Identity cid)))
  pure . Map.fromList $ do
    (subId, cs, epoch, ts, gid) <- subs
    pure
      ( subId,
        ConversationMLSData
          { cnvmlsGroupId = gid,
            cnvmlsEpoch = epoch,
            cnvmlsEpochTimestamp = epochTimestamp epoch ts,
            cnvmlsCipherSuite = cs
          }
      )

interpretSubConversationStoreToCassandra ::
  Members '[Embed IO, Input ClientState] r =>
  Sem (SubConversationStore ': r) a ->
  Sem r a
interpretSubConversationStoreToCassandra = interpret $ \case
  CreateSubConversation convId subConvId suite groupId ->
    embedClient (insertSubConversation convId subConvId suite groupId)
  GetSubConversation convId subConvId -> embedClient (selectSubConversation convId subConvId)
  GetSubConversationGroupInfo convId subConvId -> embedClient (selectSubConvGroupInfo convId subConvId)
  GetSubConversationEpoch convId subConvId -> embedClient (selectSubConvEpoch convId subConvId)
  SetSubConversationGroupInfo convId subConvId mPgs -> embedClient (updateSubConvGroupInfo convId subConvId mPgs)
  SetSubConversationEpoch cid sconv epoch -> embedClient $ setEpochForSubConversation cid sconv epoch
  SetSubConversationCipherSuite cid sconv cs -> embedClient $ setCipherSuiteForSubConversation cid sconv cs
  ListSubConversations cid -> embedClient $ listSubConversations cid
  DeleteSubConversation convId subConvId -> embedClient $ deleteSubConversation convId subConvId

--------------------------------------------------------------------------------
-- Utilities

epochTimestamp :: Epoch -> Writetime Epoch -> Maybe UTCTime
epochTimestamp (Epoch 0) _ = Nothing
epochTimestamp _ (Writetime t) = Just t
