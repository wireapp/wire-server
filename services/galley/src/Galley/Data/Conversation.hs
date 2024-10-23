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

module Galley.Data.Conversation
  ( -- * Data Conversation types
    Conversation (..),
    NewConversation,

    -- * Utilities
    isConvDeleted,
    selfConv,
    localOne2OneConvId,
    convAccess,
    convAccessData,
    convAccessRoles,
    convMessageTimer,
    convName,
    convReceiptMode,
    convSetName,
    convType,
    convSetType,
    convTeam,
    defRole,
    maybeRole,
    defRegularConvAccess,
    parseAccessRoles,
  )
where

import Data.Id
import Data.Misc
import Data.Set (Set)
import Data.Set qualified as Set
import Data.UUID.Tagged qualified as U
import Galley.Cassandra.Instances ()
import Galley.Data.Conversation.Types
import Imports hiding (Set)
import Wire.API.Conversation hiding (Conversation)

isConvDeleted :: Conversation -> Bool
isConvDeleted = convDeleted

selfConv :: UserId -> ConvId
selfConv uid = Id (toUUID uid)

-- | We deduce the conversation ID by adding the 4 components of the V4 UUID
-- together pairwise, and then setting the version bits (v4) and variant bits
-- (variant 2). This means that we always know what the UUID is for a
-- one-to-one conversation which hopefully makes them unique.
localOne2OneConvId :: U.UUID U.V4 -> U.UUID U.V4 -> ConvId
localOne2OneConvId a b = Id . U.unpack $ U.addv4 a b

convType :: Conversation -> ConvType
convType = cnvmType . convMetadata

convSetType :: ConvType -> Conversation -> Conversation
convSetType t c = c {convMetadata = (convMetadata c) {cnvmType = t}}

convTeam :: Conversation -> Maybe TeamId
convTeam = cnvmTeam . convMetadata

convAccess :: Conversation -> [Access]
convAccess = cnvmAccess . convMetadata

convAccessRoles :: Conversation -> Set AccessRole
convAccessRoles = cnvmAccessRoles . convMetadata

convAccessData :: Conversation -> ConversationAccessData
convAccessData c =
  ConversationAccessData
    (Set.fromList (convAccess c))
    (convAccessRoles c)

convName :: Conversation -> Maybe Text
convName = cnvmName . convMetadata

convSetName :: Maybe Text -> Conversation -> Conversation
convSetName n c = c {convMetadata = (convMetadata c) {cnvmName = n}}

defRegularConvAccess :: [Access]
defRegularConvAccess = [InviteAccess]

parseAccessRoles :: Maybe AccessRoleLegacy -> Maybe (Set AccessRole) -> Maybe (Set AccessRole)
parseAccessRoles mbLegacy mbAccess = mbAccess <|> fromAccessRoleLegacy <$> mbLegacy

convMessageTimer :: Conversation -> Maybe Milliseconds
convMessageTimer = cnvmMessageTimer . convMetadata

convReceiptMode :: Conversation -> Maybe ReceiptMode
convReceiptMode = cnvmReceiptMode . convMetadata
