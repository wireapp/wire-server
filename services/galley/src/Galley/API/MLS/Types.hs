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
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Qualified
import Galley.Types.Conversations.Members
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.MLS.Credential
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
type ClientMap = Map (Qualified UserId) (Map ClientId LeafIndex)

mkClientMap :: [(Domain, UserId, ClientId, Int32, Bool)] -> ClientMap
mkClientMap = foldr addEntry mempty
  where
    addEntry :: (Domain, UserId, ClientId, Int32, Bool) -> ClientMap -> ClientMap
    addEntry (dom, usr, c, leafidx, pending_removal)
      | pending_removal = id -- treat as removed, don't add to ClientMap
      | otherwise = Map.insertWith (<>) (Qualified usr dom) (Map.singleton c (fromIntegral leafidx))

cmLookupIndex :: ClientIdentity -> ClientMap -> Maybe LeafIndex
cmLookupIndex cid cm = do
  clients <- Map.lookup (cidQualifiedUser cid) cm
  Map.lookup (ciClient cid) clients

cmRemoveClient :: ClientIdentity -> ClientMap -> ClientMap
cmRemoveClient cid cm = case Map.lookup (cidQualifiedUser cid) cm of
  Nothing -> cm
  Just clients ->
    let clients' = Map.delete (ciClient cid) clients
     in if Map.null clients'
          then Map.delete (cidQualifiedUser cid) cm
          else Map.insert (cidQualifiedUser cid) clients' cm

isClientMember :: ClientIdentity -> ClientMap -> Bool
isClientMember ci = isJust . cmLookupIndex ci

cmAssocs :: ClientMap -> [(ClientIdentity, LeafIndex)]
cmAssocs cm = do
  (quid, clients) <- Map.assocs cm
  (clientId, idx) <- Map.assocs clients
  pure (mkClientIdentity quid clientId, idx)

cmIdentities :: ClientMap -> [ClientIdentity]
cmIdentities = map fst . cmAssocs

cmSingleton :: ClientIdentity -> LeafIndex -> ClientMap
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
    mcMembers :: ClientMap,
    mcIndexMap :: IndexMap
  }
  deriving (Show)

data SubConversation = SubConversation
  { scParentConvId :: ConvId,
    scSubConvId :: SubConvId,
    scMLSData :: ConversationMLSData,
    scMembers :: ClientMap,
    scIndexMap :: IndexMap
  }
  deriving (Eq, Show)

toPublicSubConv :: Qualified SubConversation -> PublicSubConversation
toPublicSubConv (Qualified (SubConversation {..}) domain) =
  let members = map fst (cmAssocs scMembers)
   in PublicSubConversation
        { pscParentConvId = Qualified scParentConvId domain,
          pscSubConvId = scSubConvId,
          pscGroupId = cnvmlsGroupId scMLSData,
          pscEpoch = cnvmlsEpoch scMLSData,
          pscEpochTimestamp = cnvmlsEpochTimestamp scMLSData,
          pscCipherSuite = cnvmlsCipherSuite scMLSData,
          pscMembers = members
        }

type ConvOrSubConv = ConvOrSubChoice MLSConversation SubConversation

mlsMetaConvOrSub :: ConvOrSubConv -> ConversationMLSData
mlsMetaConvOrSub (Conv c) = mcMLSData c
mlsMetaConvOrSub (SubConv _ s) = scMLSData s

membersConvOrSub :: ConvOrSubConv -> ClientMap
membersConvOrSub (Conv c) = mcMembers c
membersConvOrSub (SubConv _ s) = scMembers s

indexMapConvOrSub :: ConvOrSubConv -> IndexMap
indexMapConvOrSub (Conv c) = mcIndexMap c
indexMapConvOrSub (SubConv _ s) = scIndexMap s

convOfConvOrSub :: ConvOrSubChoice c s -> c
convOfConvOrSub (Conv c) = c
convOfConvOrSub (SubConv c _) = c

idForConvOrSub :: ConvOrSubConv -> ConvOrSubConvId
idForConvOrSub (Conv c) = Conv (mcId c)
idForConvOrSub (SubConv c s) = SubConv (mcId c) (scSubConvId s)
