{-# LANGUAGE StrictData #-}

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

module Galley.Types.Conversations.Members
  ( LocalMember,
    Member,
    InternalMember (..),
  )
where

import Data.Id as Id
import Data.IdMapping (MappedOrLocalId)
import Imports
import Wire.API.Conversation.Member (MutedStatus)
import Wire.API.Conversation.Role (RoleName)
import Wire.API.Provider.Service (ServiceRef)

type LocalMember = InternalMember Id.UserId

type Member = InternalMember (MappedOrLocalId Id.U)

-- | Internal representation of a conversation member.
data InternalMember id = Member
  { memId :: id,
    memService :: Maybe ServiceRef,
    -- | DEPRECATED, remove it once enough clients use `memOtrMutedStatus`
    memOtrMuted :: Bool,
    memOtrMutedStatus :: Maybe MutedStatus,
    memOtrMutedRef :: Maybe Text,
    memOtrArchived :: Bool,
    memOtrArchivedRef :: Maybe Text,
    memHidden :: Bool,
    memHiddenRef :: Maybe Text,
    memConvRoleName :: RoleName
  }
  deriving stock (Functor, Show)
