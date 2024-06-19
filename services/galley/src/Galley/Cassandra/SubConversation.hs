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

module Galley.Cassandra.SubConversation (interpretSubConversationStoreToCassandra) where

import Cassandra
import Cassandra.Util
import Control.Monad.Trans.Maybe
import Data.Id
import Data.Map qualified as Map
import Galley.API.MLS.Types
import Galley.Cassandra.Conversation.MLS
import Galley.Cassandra.Queries qualified as Cql
import Galley.Cassandra.Store (embedClient)
import Galley.Cassandra.Util
import Galley.Effects.SubConversationStore (SubConversationStore (..))
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
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
  let activeData =
        ActiveMLSConversationData
          <$> mEpoch
          <*> fmap writetimeToUTC mEpochWritetime
          <*> mSuite
  groupId <- hoistMaybe mGroupId
  (cm, im) <- lift $ lookupMLSClientLeafIndices groupId
  pure $
    SubConversation
      { scParentConvId = convId,
        scSubConvId = subConvId,
        scMLSData =
          ConversationMLSData
            { cnvmlsGroupId = groupId,
              cnvmlsActiveData = activeData
            },
        scMembers = cm,
        scIndexMap = im
      }

insertSubConversation ::
  ConvId ->
  SubConvId ->
  GroupId ->
  Client SubConversation
insertSubConversation convId subConvId groupId = do
  retry
    x5
    ( write
        Cql.insertSubConversation
        ( params
            LocalQuorum
            (convId, subConvId, Epoch 0, groupId, Nothing)
        )
    )
  pure (newSubConversation convId subConvId groupId)

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
    let activeData = case (epoch, ts) of
          (Epoch 0, _) -> Nothing
          (_, Writetime t) ->
            Just
              ActiveMLSConversationData
                { epoch = epoch,
                  epochTimestamp = t,
                  ciphersuite = cs
                }

    pure
      ( subId,
        ConversationMLSData
          { cnvmlsGroupId = gid,
            cnvmlsActiveData = activeData
          }
      )

interpretSubConversationStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  Sem (SubConversationStore ': r) a ->
  Sem r a
interpretSubConversationStoreToCassandra = interpret $ \case
  CreateSubConversation convId subConvId groupId -> do
    logEffect "SubConversationStore.CreateSubConversation"
    embedClient (insertSubConversation convId subConvId groupId)
  GetSubConversation convId subConvId -> do
    logEffect "SubConversationStore.GetSubConversation"
    embedClient (selectSubConversation convId subConvId)
  GetSubConversationGroupInfo convId subConvId -> do
    logEffect "SubConversationStore.GetSubConversationGroupInfo"
    embedClient (selectSubConvGroupInfo convId subConvId)
  GetSubConversationEpoch convId subConvId -> do
    logEffect "SubConversationStore.GetSubConversationEpoch"
    embedClient (selectSubConvEpoch convId subConvId)
  SetSubConversationGroupInfo convId subConvId mPgs -> do
    logEffect "SubConversationStore.SetSubConversationGroupInfo"
    embedClient (updateSubConvGroupInfo convId subConvId mPgs)
  SetSubConversationEpoch cid sconv epoch -> do
    logEffect "SubConversationStore.SetSubConversationEpoch"
    embedClient (setEpochForSubConversation cid sconv epoch)
  SetSubConversationCipherSuite cid sconv cs -> do
    logEffect "SubConversationStore.SetSubConversationCipherSuite"
    embedClient (setCipherSuiteForSubConversation cid sconv cs)
  ListSubConversations cid -> do
    logEffect "SubConversationStore.ListSubConversations"
    embedClient (listSubConversations cid)
  DeleteSubConversation convId subConvId -> do
    logEffect "SubConversationStore.DeleteSubConversation"
    embedClient (deleteSubConversation convId subConvId)
