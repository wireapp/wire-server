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
{-# LANGUAGE RecordWildCards #-}

module Galley.API.MLS.Types where

import Data.Domain
import Data.Id
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Map qualified as Map
import Data.Qualified
import GHC.Records (HasField (..))
import Galley.Data.Conversation.Types
import Galley.Types.Conversations.Members
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.MLS.Credential
import Wire.API.MLS.Group.Serialisation
import Wire.API.MLS.LeafNode
import Wire.API.MLS.SubConversation

-- | A map of leaf index to members.
--
-- This is used to reconstruct client
-- identities from leaf indices in remove proposals, as well as to allocate new
-- indices for added clients.
--
-- Note that clients that are in the process of being removed from a group
-- (i.e. there is a pending remove proposals for them) are included in this
-- mapping.
newtype IndexMap = IndexMap {unIndexMap :: IntMap ClientIdentity}
  deriving (Eq, Show)
  deriving newtype (Semigroup, Monoid)

mkIndexMap :: [(Domain, UserId, ClientId, Int32, Bool)] -> IndexMap
mkIndexMap = IndexMap . foldr addEntry mempty
  where
    addEntry (dom, usr, c, leafidx, _pending_removal) =
      IntMap.insert (fromIntegral leafidx) (ClientIdentity dom usr c)

imLookup :: IndexMap -> LeafIndex -> Maybe ClientIdentity
imLookup m i = IntMap.lookup (fromIntegral i) (unIndexMap m)

imNextIndex :: IndexMap -> LeafIndex
imNextIndex im =
  fromIntegral . fromJust $
    find (\n -> not $ IntMap.member n (unIndexMap im)) [0 ..]

imAddClient :: IndexMap -> ClientIdentity -> (LeafIndex, IndexMap)
imAddClient im cid = let idx = imNextIndex im in (idx, IndexMap $ IntMap.insert (fromIntegral idx) cid $ unIndexMap im)

imRemoveClient :: IndexMap -> LeafIndex -> Maybe (ClientIdentity, IndexMap)
imRemoveClient im idx = do
  cid <- imLookup im idx
  pure (cid, IndexMap . IntMap.delete (fromIntegral idx) $ unIndexMap im)

-- | A two-level map of users to clients to leaf indices.
--
-- This is used to keep track of the state of an MLS group for e.g. propagating
-- a message to all the clients that are supposed to receive it.
--
-- Note that clients that are in the process of being removed from a group
-- (i.e. there is a pending remove proposals for them) are __not__ included in
-- this mapping.
type ClientMap a = Map (Qualified UserId) (Map ClientId a)

mkClientMap :: [(Domain, UserId, ClientId, Int32, Bool)] -> ClientMap LeafIndex
mkClientMap = foldr addEntry mempty
  where
    addEntry :: (Domain, UserId, ClientId, Int32, Bool) -> ClientMap LeafIndex -> ClientMap LeafIndex
    addEntry (dom, usr, c, leafidx, pending_removal)
      | pending_removal = id -- treat as removed, don't add to ClientMap
      | otherwise = Map.insertWith (<>) (Qualified usr dom) (Map.singleton c (fromIntegral leafidx))

cmLookupIndex :: ClientIdentity -> ClientMap LeafIndex -> Maybe LeafIndex
cmLookupIndex cid cm = do
  clients <- Map.lookup (cidQualifiedUser cid) cm
  Map.lookup (ciClient cid) clients

cmRemoveClient :: ClientIdentity -> ClientMap LeafIndex -> ClientMap LeafIndex
cmRemoveClient cid cm = case Map.lookup (cidQualifiedUser cid) cm of
  Nothing -> cm
  Just clients ->
    let clients' = Map.delete (ciClient cid) clients
     in if Map.null clients'
          then Map.delete (cidQualifiedUser cid) cm
          else Map.insert (cidQualifiedUser cid) clients' cm

isClientMember :: ClientIdentity -> ClientMap LeafIndex -> Bool
isClientMember ci = isJust . cmLookupIndex ci

cmAssocs :: ClientMap a -> [(ClientIdentity, a)]
cmAssocs cm = do
  (quid, clients) <- Map.assocs cm
  (clientId, idx) <- Map.assocs clients
  pure (mkClientIdentity quid clientId, idx)

cmIdentities :: ClientMap a -> [ClientIdentity]
cmIdentities = map fst . cmAssocs

cmSingleton :: ClientIdentity -> a -> ClientMap a
cmSingleton cid idx =
  Map.singleton
    (cidQualifiedUser cid)
    (Map.singleton (ciClient cid) idx)

-- | Inform a handler for 'POST /conversations/list-ids' if the MLS global team
-- conversation and the MLS self-conversation should be included in the
-- response.
data ListGlobalSelfConvs = ListGlobalSelf | DoNotListGlobalSelf
  deriving (Eq)

data MLSConversation = MLSConversation
  { mcId :: ConvId,
    mcMetadata :: ConversationMetadata,
    mcMLSData :: ConversationMLSData,
    mcLocalMembers :: [LocalMember],
    mcRemoteMembers :: [RemoteMember],
    mcMembers :: ClientMap LeafIndex,
    mcIndexMap :: IndexMap,
    mcMigrationState :: MLSMigrationState
  }
  deriving (Show)

data SubConversation = SubConversation
  { scParentConvId :: ConvId,
    scSubConvId :: SubConvId,
    scMLSData :: ConversationMLSData,
    scMembers :: ClientMap LeafIndex,
    scIndexMap :: IndexMap
  }
  deriving (Eq, Show)

newSubConversation :: ConvId -> SubConvId -> GroupId -> SubConversation
newSubConversation convId subConvId groupId =
  SubConversation
    { scParentConvId = convId,
      scSubConvId = subConvId,
      scMLSData =
        ConversationMLSData
          { cnvmlsGroupId = groupId,
            cnvmlsActiveData = Nothing
          },
      scMembers = mkClientMap [],
      scIndexMap = mempty
    }

newSubConversationFromParent ::
  Local ConvId ->
  SubConvId ->
  SubConversation
newSubConversationFromParent lconv sconv =
  let qcs = flip SubConv sconv <$> tUntagged lconv
      groupId = newGroupId RegularConv qcs
   in newSubConversation (tUnqualified lconv) sconv groupId

toPublicSubConv :: Qualified SubConversation -> PublicSubConversation
toPublicSubConv (Qualified (SubConversation {..}) domain) =
  let members = map fst (cmAssocs scMembers)
   in PublicSubConversation
        { pscParentConvId = Qualified scParentConvId domain,
          pscSubConvId = scSubConvId,
          pscGroupId = cnvmlsGroupId scMLSData,
          pscActiveData = cnvmlsActiveData scMLSData,
          pscMembers = members
        }

type ConvOrSubConv = ConvOrSubChoice MLSConversation SubConversation

instance HasField "meta" ConvOrSubConv ConversationMetadata where
  getField x = x.conv.mcMetadata

instance HasField "mlsMeta" ConvOrSubConv ConversationMLSData where
  getField (Conv c) = mcMLSData c
  getField (SubConv _ s) = scMLSData s

instance HasField "members" ConvOrSubConv (ClientMap LeafIndex) where
  getField (Conv c) = mcMembers c
  getField (SubConv _ s) = scMembers s

instance HasField "indexMap" ConvOrSubConv IndexMap where
  getField (Conv c) = mcIndexMap c
  getField (SubConv _ s) = scIndexMap s

instance HasField "id" ConvOrSubConv ConvOrSubConvId where
  getField (Conv c) = Conv (mcId c)
  getField (SubConv c s) = SubConv (mcId c) (scSubConvId s)

instance HasField "migrationState" ConvOrSubConv MLSMigrationState where
  getField (Conv c) = c.mcMigrationState
  getField (SubConv _ _) = MLSMigrationMLS
