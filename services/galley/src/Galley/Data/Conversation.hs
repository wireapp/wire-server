-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
    isSelfConv,
    isO2OConv,
    isTeamConv,
    isConvDeleted,
    selfConv,
    localOne2OneConvId,
    convMetadata,
  )
where

import Data.Id
import qualified Data.UUID.Tagged as U
import Galley.Data.Conversation.Types
import Galley.Data.Instances ()
import Imports hiding (Set)
import Wire.API.Conversation hiding (Conversation)

isSelfConv :: Conversation -> Bool
isSelfConv = (SelfConv ==) . convType

isO2OConv :: Conversation -> Bool
isO2OConv = (One2OneConv ==) . convType

isTeamConv :: Conversation -> Bool
isTeamConv = isJust . convTeam

isConvDeleted :: Conversation -> Bool
isConvDeleted = fromMaybe False . convDeleted

selfConv :: UserId -> ConvId
selfConv uid = Id (toUUID uid)

-- | We deduce the conversation ID by adding the 4 components of the V4 UUID
-- together pairwise, and then setting the version bits (v4) and variant bits
-- (variant 2). This means that we always know what the UUID is for a
-- one-to-one conversation which hopefully makes them unique.
localOne2OneConvId :: U.UUID U.V4 -> U.UUID U.V4 -> ConvId
localOne2OneConvId a b = Id . U.unpack $ U.addv4 a b

convMetadata :: Conversation -> ConversationMetadata
convMetadata c =
  ConversationMetadata
    (convType c)
    (convCreator c)
    (convAccess c)
    (convAccessRole c)
    (convName c)
    (convTeam c)
    (convMessageTimer c)
    (convReceiptMode c)
