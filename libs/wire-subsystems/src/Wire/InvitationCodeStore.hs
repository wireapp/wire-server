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

module Wire.InvitationCodeStore where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Id (InvitationId, TeamId, UserId)
import Data.Json.Util (UTCTimeMillis)
import Data.Range (Range)
import Database.CQL.Protocol (Record (..), TupleType, recordInstance)
import Imports
import Polysemy
import Polysemy.TinyLog (TinyLog)
import System.Logger.Message qualified as Log
import URI.ByteString
import Util.Timeout
import Wire.API.Team.Invitation (Invitation (inviteeEmail))
import Wire.API.Team.Invitation qualified as Public
import Wire.API.Team.Role (Role, defaultRole)
import Wire.API.User (EmailAddress, InvitationCode, Name)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))
import Wire.Sem.Logger qualified as Log

data StoredInvitation = MkStoredInvitation
  { teamId :: TeamId,
    role :: Maybe Role,
    invitationId :: InvitationId,
    createdAt :: UTCTimeMillis,
    createdBy :: Maybe UserId,
    email :: EmailAddress,
    name :: Maybe Name,
    code :: InvitationCode
  }
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform StoredInvitation)

recordInstance ''StoredInvitation

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

recordInstance ''InsertInvitation

data PaginatedResult a
  = PaginatedResultHasMore a
  | PaginatedResult a
  deriving stock (Eq, Ord, Show, Functor, Foldable)

----------------------------

data InvitationCodeStore :: Effect where
  InsertInvitation :: InsertInvitation -> Timeout -> InvitationCodeStore m StoredInvitation
  LookupInvitation :: TeamId -> InvitationId -> InvitationCodeStore m (Maybe StoredInvitation)
  LookupInvitationByCode :: InvitationCode -> InvitationCodeStore m (Maybe StoredInvitation)
  LookupInvitationCodesByEmail :: EmailAddress -> InvitationCodeStore m [StoredInvitation]
  -- | Range is page size, it defaults to 100
  LookupInvitationsPaginated :: Maybe (Range 1 500 Int32) -> TeamId -> Maybe InvitationId -> InvitationCodeStore m (PaginatedResult [StoredInvitation])
  CountInvitations :: TeamId -> InvitationCodeStore m Int64
  DeleteInvitation :: TeamId -> InvitationId -> InvitationCodeStore m ()
  DeleteAllTeamInvitations :: TeamId -> InvitationCodeStore m ()

makeSem ''InvitationCodeStore

----------------------------

lookupInvitationByEmail :: (Member InvitationCodeStore r, Member TinyLog r) => EmailAddress -> Sem r (Maybe StoredInvitation)
lookupInvitationByEmail email = runMaybeT do
  inv <- MaybeT $ lookupSingleInvitationCodeByEmail email
  MaybeT $ lookupInvitation inv.teamId inv.invitationId

lookupSingleInvitationCodeByEmail :: (Member TinyLog r, Member InvitationCodeStore r) => EmailAddress -> Sem r (Maybe StoredInvitation)
lookupSingleInvitationCodeByEmail email = do
  invs <- lookupInvitationCodesByEmail email
  case invs of
    [] -> pure Nothing
    [inv] -> pure $ Just inv
    (_ : _ : _) -> do
      -- edge case: more than one pending invite from different teams
      Log.info $
        Log.msg (Log.val "team_invidation_email: multiple pending invites from different teams for the same email")
          . Log.field "email" (show email)

      pure Nothing

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
