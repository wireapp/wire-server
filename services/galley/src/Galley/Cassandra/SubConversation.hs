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

module Galley.Cassandra.SubConversation where

import Cassandra
import Data.Id
import Data.Qualified
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
  for m $ \(suite, epoch, groupId) -> do
    cm <- lookupMLSClients groupId
    pure $
      SubConversation
        { scParentConvId = convId,
          scSubConvId = subConvId,
          scMLSData =
            ConversationMLSData
              { cnvmlsGroupId = groupId,
                cnvmlsEpoch = epoch,
                cnvmlsCipherSuite = suite
              },
          scMembers = cm
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

deletePublicGroupState :: ConvId -> SubConvId -> Client ()
deletePublicGroupState convId subConvId =
  retry x5 $ write Cql.deletePublicGroupState (params LocalQuorum (convId, subConvId))

interpretSubConversationStoreToCassandra ::
  Members '[Embed IO, Input ClientState] r =>
  Sem (SubConversationStore ': r) a ->
  Sem r a
interpretSubConversationStoreToCassandra = interpret $ \case
  GetSubConversation convId subConvId -> embedClient (selectSubConversation convId subConvId)
  CreateSubConversation convId subConvId suite epoch groupId mPgs -> embedClient (insertSubConversation convId subConvId suite epoch groupId mPgs)
  SetSubConversationPublicGroupState convId subConvId mPgs -> embedClient (updateSubConvPublicGroupState convId subConvId mPgs)
  GetSubConversationPublicGroupState convId subConvId -> embedClient (selectSubConvPublicGroupState convId subConvId)
  SetGroupIdForSubConversation gId cid sconv -> embedClient $ setGroupIdForSubConversation gId cid sconv
  SetSubConversationEpoch cid sconv epoch -> embedClient $ setEpochForSubConversation cid sconv epoch
  DeleteSubConversationPublicGroupState convId subConvId -> embedClient $ deletePublicGroupState convId subConvId
