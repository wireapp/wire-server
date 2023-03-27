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
import Data.Id
import qualified Data.Map as Map
import Data.Qualified
import Data.Time.Clock
import Galley.API.MLS.Types (SubConversation (..))
import Galley.Cassandra.Conversation.MLS (lookupMLSClients)
import qualified Galley.Cassandra.Queries as Cql
import Galley.Cassandra.Store (embedClient)
import Galley.Effects.SubConversationStore (SubConversationStore (..))
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Conversation.Protocol
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Group
import Wire.API.MLS.PublicGroupState
import Wire.API.MLS.SubConversation

selectSubConversation :: ConvId -> SubConvId -> Client (Maybe SubConversation)
selectSubConversation convId subConvId = do
  m <- retry x5 (query1 Cql.selectSubConversation (params LocalQuorum (convId, subConvId)))
  for m $ \(suite, epoch, epochWritetime, groupId) -> do
    cm <- lookupMLSClients groupId
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
          scIndexMap = mempty -- TODO
        }

insertSubConversation :: ConvId -> SubConvId -> CipherSuiteTag -> Epoch -> GroupId -> Maybe OpaquePublicGroupState -> Client ()
insertSubConversation convId subConvId suite epoch groupId mPgs =
  retry x5 (write Cql.insertSubConversation (params LocalQuorum (convId, subConvId, suite, epoch, groupId, mPgs)))

updateSubConvPublicGroupState :: ConvId -> SubConvId -> Maybe OpaquePublicGroupState -> Client ()
updateSubConvPublicGroupState convId subConvId mPgs =
  retry x5 (write Cql.updateSubConvPublicGroupState (params LocalQuorum (convId, subConvId, mPgs)))

selectSubConvPublicGroupState :: ConvId -> SubConvId -> Client (Maybe OpaquePublicGroupState)
selectSubConvPublicGroupState convId subConvId =
  (runIdentity =<<) <$> retry x5 (query1 Cql.selectSubConvPublicGroupState (params LocalQuorum (convId, subConvId)))

setGroupIdForSubConversation :: GroupId -> Qualified ConvId -> SubConvId -> Client ()
setGroupIdForSubConversation groupId qconv sconv =
  retry x5 (write Cql.insertGroupIdForSubConversation (params LocalQuorum (groupId, qUnqualified qconv, qDomain qconv, sconv)))

setEpochForSubConversation :: ConvId -> SubConvId -> Epoch -> Client ()
setEpochForSubConversation cid sconv epoch =
  retry x5 (write Cql.insertEpochForSubConversation (params LocalQuorum (epoch, cid, sconv)))

deleteGroupId :: GroupId -> Client ()
deleteGroupId groupId =
  retry x5 $ write Cql.deleteGroupId (params LocalQuorum (Identity groupId))

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
  CreateSubConversation convId subConvId suite epoch groupId mPgs -> embedClient (insertSubConversation convId subConvId suite epoch groupId mPgs)
  GetSubConversation convId subConvId -> embedClient (selectSubConversation convId subConvId)
  GetSubConversationPublicGroupState convId subConvId -> embedClient (selectSubConvPublicGroupState convId subConvId)
  SetSubConversationPublicGroupState convId subConvId mPgs -> embedClient (updateSubConvPublicGroupState convId subConvId mPgs)
  SetGroupIdForSubConversation gId cid sconv -> embedClient $ setGroupIdForSubConversation gId cid sconv
  SetSubConversationEpoch cid sconv epoch -> embedClient $ setEpochForSubConversation cid sconv epoch
  DeleteGroupIdForSubConversation groupId -> embedClient $ deleteGroupId groupId
  ListSubConversations cid -> embedClient $ listSubConversations cid
  DeleteSubConversation convId subConvId -> embedClient $ deleteSubConversation convId subConvId

--------------------------------------------------------------------------------
-- Utilities

epochTimestamp :: Epoch -> Writetime Epoch -> Maybe UTCTime
epochTimestamp (Epoch 0) _ = Nothing
epochTimestamp _ (Writetime t) = Just t
