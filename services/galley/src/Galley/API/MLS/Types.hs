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

import qualified Crypto.Hash as Crypto
import Data.ByteArray (convert)
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import Galley.Data.Conversation
import qualified Galley.Data.Conversation as Data
import Imports
import Wire.API.Conversation.Protocol
import Wire.API.MLS.Credential
import Wire.API.MLS.Group
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.SubConversation

type ClientMap = Map (Qualified UserId) (Set (ClientId, KeyPackageRef))

mkClientMap :: [(Domain, UserId, ClientId, KeyPackageRef)] -> ClientMap
mkClientMap = foldr addEntry mempty
  where
    addEntry :: (Domain, UserId, ClientId, KeyPackageRef) -> ClientMap -> ClientMap
    addEntry (dom, usr, c, kpr) =
      Map.insertWith (<>) (Qualified usr dom) (Set.singleton (c, kpr))

cmAssocs :: ClientMap -> [(Qualified UserId, (ClientId, KeyPackageRef))]
cmAssocs cm = Map.assocs cm >>= traverse toList

-- | Inform a handler for 'POST /conversations/list-ids' if the MLS global team
-- conversation and the MLS self-conversation should be included in the
-- response.
data ListGlobalSelfConvs = ListGlobalSelf | DoNotListGlobalSelf
  deriving (Eq)

data MLSConversation = MLSConversation
  { mcConv :: Conversation,
    mcMLSData :: ConversationMLSData,
    mcMembers :: ClientMap
  }
  deriving (Show)

data SubConversation = SubConversation
  { scParentConvId :: Local ConvId,
    scSubConvId :: SubConvId,
    scMLSData :: ConversationMLSData,
    scMembers :: ClientMap
  }
  deriving (Eq, Show)

toPublicSubConv :: SubConversation -> PublicSubConversation
toPublicSubConv SubConversation {..} =
  let members = fmap (\(quid, (cid, _kp)) -> mkClientIdentity quid cid) (cmAssocs scMembers)
   in PublicSubConversation
        { pscParentConvId = tUntagged scParentConvId,
          pscSubConvId = scSubConvId,
          pscGroupId = cnvmlsGroupId scMLSData,
          pscEpoch = cnvmlsEpoch scMLSData,
          pscCipherSuite = cnvmlsCipherSuite scMLSData,
          pscMembers = members
        }

initialGroupId :: Local ConvId -> SubConvId -> GroupId
initialGroupId lcnv sconv =
  GroupId
    . convert
    . Crypto.hash @ByteString @Crypto.SHA256
    $ toByteString' (tUnqualified lcnv)
      <> toByteString' (tDomain lcnv)
      <> toByteString' (unSubConvId sconv)

type ConvOrSubConv = ConvOrSubChoice MLSConversation SubConversation

mlsMetaConvOrSub :: ConvOrSubConv -> ConversationMLSData
mlsMetaConvOrSub (Conv c) = mcMLSData c
mlsMetaConvOrSub (SubConv _ s) = scMLSData s

membersConvOrSub :: ConvOrSubConv -> ClientMap
membersConvOrSub (Conv c) = mcMembers c
membersConvOrSub (SubConv _ s) = scMembers s

convOfConvOrSub :: ConvOrSubChoice c s -> c
convOfConvOrSub (Conv c) = c
convOfConvOrSub (SubConv c _) = c

idForConvOrSub :: ConvOrSubConv -> ConvOrSubConvId
idForConvOrSub (Conv c) = Conv (Data.convId . mcConv $ c)
idForConvOrSub (SubConv c s) = SubConv (Data.convId . mcConv $ c) (scSubConvId s)
