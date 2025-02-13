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
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.InvitationStore where

import Data.Id (InvitationId, TeamId, UserId)
import Data.Json.Util (UTCTimeMillis, toUTCTimeMillis)
import Data.Range (Range)
import Database.CQL.Protocol (Record (..), TupleType, recordInstance)
import Imports
import Polysemy
import URI.ByteString
import Util.Timeout
import Wire.API.Team.Invitation (Invitation (inviteeEmail))
import Wire.API.Team.Invitation qualified as Public
import Wire.API.Team.Role (Role, defaultRole)
import Wire.API.User (EmailAddress, InvitationCode, Name)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

data StoredInvitation = MkStoredInvitation
  { teamId :: TeamId,
    role :: Maybe Role,
    invitationId :: InvitationId,
    createdAt :: UTCTimeMillis,
    createdBy :: Maybe UserId,
    -- | The invitee's email address
    email :: EmailAddress,
    name :: Maybe Name,
    code :: InvitationCode
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform StoredInvitation)

recordInstance ''StoredInvitation

-- | The difference between this and `StoredInvitation` is the type of `createdAt` (plus
-- reordering and renaming of fields for some reason).  See 'insertInvToStoredInv' below.
data InsertInvitation = MkInsertInvitation
  { invitationId :: InvitationId,
    teamId :: TeamId,
    role :: Role,
    createdAt :: UTCTime,
    createdBy :: Maybe UserId,
    inviteeEmail :: EmailAddress,
    inviteeName :: Maybe Name,
    code :: InvitationCode
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform InsertInvitation)

recordInstance ''InsertInvitation

data PaginatedResult a
  = PaginatedResultHasMore a
  | PaginatedResult a
  deriving stock (Eq, Ord, Show, Functor, Foldable)

----------------------------

data InvitationStore :: Effect where
  InsertInvitation :: InsertInvitation -> Timeout -> InvitationStore m StoredInvitation
  LookupInvitation :: TeamId -> InvitationId -> InvitationStore m (Maybe StoredInvitation)
  LookupInvitationByCode :: InvitationCode -> InvitationStore m (Maybe StoredInvitation)
  LookupInvitationsByEmail :: EmailAddress -> InvitationStore m [StoredInvitation]
  -- | Range is page size, it defaults to 100
  LookupInvitationsPaginated :: Maybe (Range 1 500 Int32) -> TeamId -> Maybe InvitationId -> InvitationStore m (PaginatedResult [StoredInvitation])
  CountInvitations :: TeamId -> InvitationStore m Int64
  DeleteInvitation :: TeamId -> InvitationId -> InvitationStore m ()
  DeleteAllTeamInvitations :: TeamId -> InvitationStore m ()

makeSem ''InvitationStore

----------------------------

invitationFromStored :: Maybe (URIRef Absolute) -> StoredInvitation -> Public.Invitation
invitationFromStored maybeUrl MkStoredInvitation {..} =
  Public.Invitation
    { team = teamId,
      role = fromMaybe defaultRole role,
      invitationId = invitationId,
      createdAt = createdAt,
      createdBy = createdBy,
      inviteeEmail = email,
      inviteeName = name,
      inviteeUrl = maybeUrl
    }

insertInvToStoredInv :: InsertInvitation -> StoredInvitation
insertInvToStoredInv (MkInsertInvitation invId teamId role (toUTCTimeMillis -> now) uid email name code) =
  MkStoredInvitation
    { teamId = teamId,
      role = Just role,
      invitationId = invId,
      createdAt = now,
      createdBy = uid,
      email = email,
      name = name,
      code = code
    }
