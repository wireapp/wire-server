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
import Wire.API.MLS.SubConversation

newtype IndexMap = IndexMap {unIndexMap :: IntMap ClientIdentity}
  deriving (Eq, Show)
  deriving newtype (Semigroup, Monoid)

indexToClient :: IndexMap -> Word32 -> Maybe ClientIdentity
indexToClient m i = IntMap.lookup (fromIntegral i) (unIndexMap m)

type ClientMap = Map (Qualified UserId) (Map ClientId Word32)

mkClientMap :: [(Domain, UserId, ClientId, Int32)] -> ClientMap
mkClientMap = foldr addEntry mempty
  where
    addEntry :: (Domain, UserId, ClientId, Int32) -> ClientMap -> ClientMap
    addEntry (dom, usr, c, kpi) =
      Map.insertWith (<>) (Qualified usr dom) (Map.singleton c (fromIntegral kpi))

cmLookupIndex :: ClientIdentity -> ClientMap -> Maybe Word32
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

cmAssocs :: ClientMap -> [(ClientIdentity, Word32)]
cmAssocs cm = do
  (quid, clients) <- Map.assocs cm
  (clientId, idx) <- Map.assocs clients
  pure (mkClientIdentity quid clientId, idx)

cmSingleton :: ClientIdentity -> Word32 -> ClientMap
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

indicesConvOrSub :: ConvOrSubConv -> IndexMap
indicesConvOrSub (Conv c) = mcIndexMap c
indicesConvOrSub (SubConv _ s) = scIndexMap s

convOfConvOrSub :: ConvOrSubChoice c s -> c
convOfConvOrSub (Conv c) = c
convOfConvOrSub (SubConv c _) = c

idForConvOrSub :: ConvOrSubConv -> ConvOrSubConvId
idForConvOrSub (Conv c) = Conv (mcId c)
idForConvOrSub (SubConv c s) = SubConv (mcId c) (scSubConvId s)
