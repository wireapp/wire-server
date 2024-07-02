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

module Galley.API.MLS.Conversation
  ( mkMLSConversation,
    newMLSConversation,
    mcConv,
  )
where

import Data.Id
import Data.Qualified
import Galley.API.MLS.Types
import Galley.Data.Conversation.Types as Data
import Galley.Effects.MemberStore
import Imports
import Polysemy
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol

mkMLSConversation ::
  (Member MemberStore r) =>
  Data.Conversation ->
  Sem r (Maybe MLSConversation)
mkMLSConversation conv =
  for (Data.mlsMetadata conv) $ \(mlsData, migrationState) -> do
    (cm, im) <- lookupMLSClientLeafIndices (cnvmlsGroupId mlsData)
    pure
      MLSConversation
        { mcId = Data.convId conv,
          mcMetadata = Data.convMetadata conv,
          mcLocalMembers = Data.convLocalMembers conv,
          mcRemoteMembers = Data.convRemoteMembers conv,
          mcMLSData = mlsData,
          mcMembers = cm,
          mcIndexMap = im,
          mcMigrationState = migrationState
        }

-- | Creates a new MLS conversation with members but no clients.
newMLSConversation :: Local ConvId -> ConversationMetadata -> ConversationMLSData -> MLSConversation
newMLSConversation lcnv meta mlsData =
  MLSConversation
    { mcId = tUnqualified lcnv,
      mcMetadata = meta,
      mcMLSData = mlsData,
      mcLocalMembers = [],
      mcRemoteMembers = [],
      mcMembers = mempty,
      mcIndexMap = mempty,
      mcMigrationState = MLSMigrationMLS
    }

mcConv :: MLSConversation -> Data.Conversation
mcConv mlsConv =
  Data.Conversation
    { convId = mcId mlsConv,
      convLocalMembers = mcLocalMembers mlsConv,
      convRemoteMembers = mcRemoteMembers mlsConv,
      convDeleted = False,
      convMetadata = mcMetadata mlsConv,
      convProtocol = ProtocolMLS (mcMLSData mlsConv)
    }
