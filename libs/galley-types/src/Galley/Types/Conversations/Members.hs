{-# LANGUAGE StrictData #-}

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

module Galley.Types.Conversations.Members
  ( RemoteMember (..),
    remoteMemberToOther,
    LocalMember (..),
    localMemberToOther,
    newMember,
    newMemberWithRole,
    MemberStatus (..),
    defMemberStatus,
  )
where

import Data.Domain
import Data.Id as Id
import Data.Qualified
import qualified Data.Set as Set
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Role (RoleName, roleNameWireAdmin)
import Wire.API.Provider.Service (ServiceRef)

-- | Internal (cassandra) representation of a remote conversation member.
data RemoteMember = RemoteMember
  { rmId :: Remote UserId,
    rmConvRoleName :: RoleName,
    rmMLSClients :: Set ClientId
  }
  deriving stock (Show)

remoteMemberToOther :: RemoteMember -> OtherMember
remoteMemberToOther x =
  OtherMember
    { omQualifiedId = qUntagged (rmId x),
      omService = Nothing,
      omConvRoleName = rmConvRoleName x
    }

-- | Internal (cassandra) representation of a local conversation member.
data LocalMember = LocalMember
  { lmId :: UserId,
    lmStatus :: MemberStatus,
    lmService :: Maybe ServiceRef,
    lmConvRoleName :: RoleName,
    lmMLSClients :: Set ClientId
  }
  deriving stock (Show)

newMember :: UserId -> LocalMember
newMember u = newMemberWithRole (u, roleNameWireAdmin)

newMemberWithRole :: (UserId, RoleName) -> LocalMember
newMemberWithRole (u, r) =
  LocalMember
    { lmId = u,
      lmService = Nothing,
      lmStatus = defMemberStatus,
      lmConvRoleName = r,
      lmMLSClients = Set.empty
    }

localMemberToOther :: Domain -> LocalMember -> OtherMember
localMemberToOther domain x =
  OtherMember
    { omQualifiedId = Qualified (lmId x) domain,
      omService = lmService x,
      omConvRoleName = lmConvRoleName x
    }

data MemberStatus = MemberStatus
  { msOtrMutedStatus :: Maybe MutedStatus,
    msOtrMutedRef :: Maybe Text,
    msOtrArchived :: Bool,
    msOtrArchivedRef :: Maybe Text,
    msHidden :: Bool,
    msHiddenRef :: Maybe Text
  }
  deriving stock (Show)

defMemberStatus :: MemberStatus
defMemberStatus =
  MemberStatus
    { msOtrMutedStatus = Nothing,
      msOtrMutedRef = Nothing,
      msOtrArchived = False,
      msOtrArchivedRef = Nothing,
      msHidden = False,
      msHiddenRef = Nothing
    }
