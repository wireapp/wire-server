{-# LANGUAGE DerivingVia #-}
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

module Wire.API.Team.Role
  ( Role (..),
    defaultRole,

    -- * Swagger
    typeRole,
  )
where

import qualified Cassandra as Cql
import Data.Aeson
import Data.Attoparsec.ByteString (takeLazyByteString)
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..))
import Data.ByteString.Conversion.From (runParser)
import Data.ByteString.Lazy.Builder (toLazyByteString)
import Data.String.Conversions (cs)
import qualified Data.Swagger.Model.Api as Doc
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

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
-- For instance, here: https://github.com/wireapp/wire-webapp/blob/dev/app/script/team/TeamPermission.js
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

-- | Team-level role.  Analog to conversation-level 'ConversationRole'.
data Role = RoleOwner | RoleAdmin | RoleMember | RoleExternalPartner
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving (Arbitrary) via (GenericUniform Role)

typeRole :: Doc.DataType
typeRole =
  Doc.Prim $
    Doc.Primitive
      { Doc.primType = Doc.PrimString,
        Doc.defaultValue = Just defaultRole,
        Doc.enum = Just [minBound ..],
        Doc.minVal = Just minBound,
        Doc.maxVal = Just maxBound
      }

instance ToJSON Role where
  toJSON = String . cs . toLazyByteString . builder

instance FromJSON Role where
  parseJSON = withText "Role" $ \str ->
    case runParser (parser @Role) (cs str) of
      Left err -> fail err
      Right result -> pure result

instance ToByteString Role where
  builder RoleOwner = "owner"
  builder RoleAdmin = "admin"
  builder RoleMember = "member"
  builder RoleExternalPartner = "partner"

instance FromByteString Role where
  parser =
    takeLazyByteString >>= \case
      "owner" -> pure RoleOwner
      "admin" -> pure RoleAdmin
      "member" -> pure RoleMember
      "partner" -> pure RoleExternalPartner
      bad -> fail ("not a role:  " <> show bad)

defaultRole :: Role
defaultRole = RoleMember

instance Cql.Cql Role where
  ctype = Cql.Tagged Cql.IntColumn

  toCql RoleOwner = Cql.CqlInt 1
  toCql RoleAdmin = Cql.CqlInt 2
  toCql RoleMember = Cql.CqlInt 3
  toCql RoleExternalPartner = Cql.CqlInt 4

  fromCql (Cql.CqlInt i) = case i of
    1 -> return RoleOwner
    2 -> return RoleAdmin
    3 -> return RoleMember
    4 -> return RoleExternalPartner
    n -> Left $ "Unexpected Role value: " ++ show n
  fromCql _ = Left "Role value: int expected"
