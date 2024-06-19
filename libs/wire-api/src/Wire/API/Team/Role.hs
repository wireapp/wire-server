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

module Wire.API.Team.Role
  ( Role (..),
    defaultRole,
  )
where

import Cassandra qualified as Cql
import Control.Error (note)
import Control.Lens ((?~))
import Data.Aeson
import Data.Attoparsec.ByteString.Char8 (string)
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..))
import Data.OpenApi qualified as S
import Data.Schema
import Data.Text qualified as T
import Imports
import Servant.API (FromHttpApiData, parseQueryParam)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

-- Note [team roles]
-- ~~~~~~~~~~~~
--
-- Client apps have a notion of *team roles*. They are defined as sets of
-- permissions:
--
--     member =
--         {AddRemoveConvMember, Create/DeleteConversation,
--         GetMemberPermissions, GetTeamConversations}
--
--     admin = member +
--         {Add/RemoveTeamMember, SetMemberPermissions, SetTeamData}
--
--     owner = admin +
--         {DeleteTeam, Get/SetBilling}
--
-- Whenever a user has one of those specific sets of permissions, they are
-- considered a member/admin/owner and the client treats them accordingly
-- (e.g. for an admin it might show a certain button, while for an ordinary
-- user it won't).
--
-- On the backend, however, we don't have such a notion. Instead we have
-- granular (in fact, probably *too* granular) permission masks. Look at
-- 'Perm' and 'Permissions'.
--
-- Admins as a concept don't exist at all, and team owners are defined as
-- "full bitmask". When we do checks like "the backend must not let the last
-- team owner leave the team", this is what we test for. We also never test
-- for "team admin", and instead look at specific permissions.
--
-- Creating a new permission flag is thus very tricky, because if we decide
-- that all team admins must have this new permission, we will have to
-- identify all existing team admins. And if it turns out that some users
-- don't fit into one of those three team roles, we're screwed.
--
-- SOLUTION: we introduce 'HiddenPerm' and 'HiddenPermissions', map
-- (non-hidden) -- permission masks to roles and roles to permissions (both
-- hidden and non-hidden), and provide a type class 'IsPerm' that handles
-- both hidden and non-hidden permissions uniformly.  We still cannot update
-- 'Perms' and 'Permissions', but we can introduce new HiddenPermissions and
-- associate them with roles.

-- | Team-level role.  Analog to conversation-level 'ConversationRole'.
data Role = RoleOwner | RoleAdmin | RoleMember | RoleExternalPartner
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving (Arbitrary) via (GenericUniform Role)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema Role

instance ToSchema Role where
  schema =
    enum @Text "Role" $
      flip foldMap [minBound .. maxBound] $ \r ->
        element (roleName r) r

instance S.ToParamSchema Role where
  toParamSchema _ =
    mempty
      & S.type_ ?~ S.OpenApiString
      & S.enum_ ?~ fmap roleName [minBound .. maxBound]

instance FromHttpApiData Role where
  parseQueryParam name = note ("Unknown role: " <> name) $
    getAlt $
      flip foldMap [minBound .. maxBound] $ \s ->
        guard (T.pack (show s) == name) $> s

roleName :: (IsString a) => Role -> a
roleName RoleOwner = "owner"
roleName RoleAdmin = "admin"
roleName RoleMember = "member"
roleName RoleExternalPartner = "partner"

instance ToByteString Role where
  builder = roleName

instance FromByteString Role where
  parser =
    asum $
      [minBound .. maxBound] <&> \ctor ->
        ctor <$ string (roleName ctor)

defaultRole :: Role
defaultRole = RoleMember

instance Cql.Cql Role where
  ctype = Cql.Tagged Cql.IntColumn

  toCql RoleOwner = Cql.CqlInt 1
  toCql RoleAdmin = Cql.CqlInt 2
  toCql RoleMember = Cql.CqlInt 3
  toCql RoleExternalPartner = Cql.CqlInt 4

  fromCql (Cql.CqlInt i) = case i of
    1 -> pure RoleOwner
    2 -> pure RoleAdmin
    3 -> pure RoleMember
    4 -> pure RoleExternalPartner
    n -> Left $ "Unexpected Role value: " ++ show n
  fromCql _ = Left "Role value: int expected"
