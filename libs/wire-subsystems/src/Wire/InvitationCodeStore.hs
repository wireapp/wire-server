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
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.InvitationCodeStore where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Id
import Data.Json.Util
import Database.CQL.Protocol (Record (..), TupleType, recordInstance)
import Imports
import Polysemy
import Wire.API.Team.Role
import Wire.API.User
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

data StoredInvitation = MkStoredInvitation
  { teamId :: TeamId,
    mrole :: Maybe Role,
    invId :: InvitationId,
    createdAt :: UTCTimeMillis,
    mcreatedBy :: Maybe UserId,
    email :: Email,
    mname :: Maybe Name,
    code :: InvitationCode
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform StoredInvitation)

recordInstance ''StoredInvitation

data StoredInvitationByTeam = MkStoredInvitationByTeam
  { teamId :: TeamId,
    invId :: InvitationId,
    code :: InvitationCode
    -- TODO(mangoiv): maybe we can drop this last element
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform StoredInvitationByTeam)

recordInstance ''StoredInvitationByTeam

data InvitationCodeStore :: Effect where
  LookupInvitation :: TeamId -> InvitationId -> InvitationCodeStore m (Maybe StoredInvitation)
  LookupInvitationCodesByEmail :: Email -> InvitationCodeStore m [StoredInvitationByTeam]
  LookupSingleInvitationCodeByEmail :: Email -> InvitationCodeStore m (Maybe StoredInvitationByTeam)

makeSem ''InvitationCodeStore

lookupInvitationByEmail :: (Member InvitationCodeStore r) => Email -> Sem r (Maybe StoredInvitation)
lookupInvitationByEmail email = runMaybeT do
  MkStoredInvitationByTeam {teamId, invId} <- MaybeT $ lookupSingleInvitationCodeByEmail email
  MaybeT $ lookupInvitation teamId invId
