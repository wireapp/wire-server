{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.API.Team.Permission
  ( Permissions,
    self,
    copy,
    newPermissions,
    fullPermissions,
    noPermissions,
    serviceWhitelistPermissions,
    Perm (..),
    permToInt,
    permsToInt,
    intToPerm,
    intToPerms,
    Role (..),
    defaultRole,
    rolePermissions,
    permissionsRole,
  )
where

import qualified Cassandra as Cql
import qualified Control.Error.Util as Err
import Control.Lens ((^.), makeLenses)
import Data.Aeson
import Data.Bits ((.|.), testBit)
import Data.Json.Util
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Imports

data Permissions = Permissions
  { _self :: Set Perm,
    _copy :: Set Perm
  }
  deriving (Eq, Ord, Show, Generic)

-- | Team-level permission.  Analog to conversation-level 'Action'.
data Perm
  = CreateConversation
  | DoNotUseDeprecatedDeleteConversation -- NOTE: This gets now overruled by conv level checks
  | AddTeamMember
  | RemoveTeamMember
  | DoNotUseDeprecatedAddRemoveConvMember -- NOTE: This gets now overruled by conv level checks
  | DoNotUseDeprecatedModifyConvName -- NOTE: This gets now overruled by conv level checks
  | GetBilling
  | SetBilling
  | SetTeamData
  | GetMemberPermissions
  | SetMemberPermissions
  | GetTeamConversations
  | DeleteTeam
  -- FUTUREWORK: make the verbs in the roles more consistent
  -- (CRUD vs. Add,Remove vs; Get,Set vs. Create,Delete etc).
  -- If you ever think about adding a new permission flag,
  -- read Note [team roles] first.
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- | Team-level role.  Analog to conversation-level 'ConversationRole'.
data Role = RoleOwner | RoleAdmin | RoleMember | RoleExternalPartner
  deriving (Eq, Show, Enum, Bounded, Generic)

defaultRole :: Role
defaultRole = RoleMember

rolePermissions :: Role -> Permissions
rolePermissions role = Permissions p p where p = rolePerms role

permissionsRole :: Permissions -> Maybe Role
permissionsRole (Permissions p p') | p /= p' = Nothing
permissionsRole (Permissions p _) = permsRole p
  where
    permsRole :: Set Perm -> Maybe Role
    permsRole perms =
      Maybe.listToMaybe
        [role | role <- [minBound ..], rolePerms role == perms]

-- | Internal function for 'rolePermissions'.  (It works iff the two sets in 'Permissions' are
-- identical for every 'Role', otherwise it'll need to be specialized for the resp. sides.)
rolePerms :: Role -> Set Perm
rolePerms RoleOwner =
  rolePerms RoleAdmin
    <> Set.fromList
      [ GetBilling,
        SetBilling,
        DeleteTeam
      ]
rolePerms RoleAdmin =
  rolePerms RoleMember
    <> Set.fromList
      [ AddTeamMember,
        RemoveTeamMember,
        SetTeamData,
        SetMemberPermissions
      ]
rolePerms RoleMember =
  rolePerms RoleExternalPartner
    <> Set.fromList
      [ DoNotUseDeprecatedDeleteConversation,
        DoNotUseDeprecatedAddRemoveConvMember,
        DoNotUseDeprecatedModifyConvName,
        GetMemberPermissions
      ]
rolePerms RoleExternalPartner =
  Set.fromList
    [ CreateConversation,
      GetTeamConversations
    ]

makeLenses ''Permissions

newPermissions ::
  -- | User's permissions
  Set Perm ->
  -- | Permissions that the user will be able to
  --   grant to other users (must be a subset)
  Set Perm ->
  Maybe Permissions
newPermissions a b
  | b `Set.isSubsetOf` a = Just (Permissions a b)
  | otherwise = Nothing

fullPermissions :: Permissions
fullPermissions = let p = intToPerms maxBound in Permissions p p

noPermissions :: Permissions
noPermissions = Permissions mempty mempty

-- | Permissions that a user needs to be considered a "service whitelist
-- admin" (can add and remove services from the whitelist).
serviceWhitelistPermissions :: Set Perm
serviceWhitelistPermissions =
  Set.fromList
    [ AddTeamMember,
      RemoveTeamMember,
      DoNotUseDeprecatedAddRemoveConvMember,
      SetTeamData
    ]

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

permToInt :: Perm -> Word64
permToInt CreateConversation = 0x0001
permToInt DoNotUseDeprecatedDeleteConversation = 0x0002
permToInt AddTeamMember = 0x0004
permToInt RemoveTeamMember = 0x0008
permToInt DoNotUseDeprecatedAddRemoveConvMember = 0x0010
permToInt DoNotUseDeprecatedModifyConvName = 0x0020
permToInt GetBilling = 0x0040
permToInt SetBilling = 0x0080
permToInt SetTeamData = 0x0100
permToInt GetMemberPermissions = 0x0200
permToInt GetTeamConversations = 0x0400
permToInt DeleteTeam = 0x0800
permToInt SetMemberPermissions = 0x1000

intToPerm :: Word64 -> Maybe Perm
intToPerm 0x0001 = Just CreateConversation
intToPerm 0x0002 = Just DoNotUseDeprecatedDeleteConversation
intToPerm 0x0004 = Just AddTeamMember
intToPerm 0x0008 = Just RemoveTeamMember
intToPerm 0x0010 = Just DoNotUseDeprecatedAddRemoveConvMember
intToPerm 0x0020 = Just DoNotUseDeprecatedModifyConvName
intToPerm 0x0040 = Just GetBilling
intToPerm 0x0080 = Just SetBilling
intToPerm 0x0100 = Just SetTeamData
intToPerm 0x0200 = Just GetMemberPermissions
intToPerm 0x0400 = Just GetTeamConversations
intToPerm 0x0800 = Just DeleteTeam
intToPerm 0x1000 = Just SetMemberPermissions
intToPerm _ = Nothing

intToPerms :: Word64 -> Set Perm
intToPerms n =
  let perms = [2 ^ i | i <- [0 .. 62], n `testBit` i]
   in Set.fromList (mapMaybe intToPerm perms)

permsToInt :: Set Perm -> Word64
permsToInt = Set.foldr' (\p n -> n .|. permToInt p) 0

instance ToJSON Permissions where
  toJSON p =
    object $
      "self" .= permsToInt (_self p)
        # "copy" .= permsToInt (_copy p)
        # []

instance FromJSON Permissions where
  parseJSON = withObject "permissions" $ \o -> do
    s <- intToPerms <$> o .: "self"
    d <- intToPerms <$> o .: "copy"
    case newPermissions s d of
      Nothing -> fail "invalid permissions"
      Just ps -> pure ps

instance ToJSON Role where
  toJSON RoleOwner = "owner"
  toJSON RoleAdmin = "admin"
  toJSON RoleMember = "member"
  toJSON RoleExternalPartner = "partner"

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "owner" -> pure RoleOwner
    "admin" -> pure RoleAdmin
    "member" -> pure RoleMember
    "partner" -> pure RoleExternalPartner
    "collaborator" -> pure RoleExternalPartner
    -- 'collaborator' was used for a short period of time on staging.  if you are
    -- wondering about this, it's probably safe to remove.
    -- ~fisx, Wed Jan 23 16:38:52 CET 2019
    bad -> fail $ "not a role: " <> show bad

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
    n -> fail $ "Unexpected Role value: " ++ show n
  fromCql _ = fail "Role value: int expected"

instance Cql.Cql Permissions where
  ctype = Cql.Tagged $ Cql.UdtColumn "permissions" [("self", Cql.BigIntColumn), ("copy", Cql.BigIntColumn)]

  toCql p =
    let f = Cql.CqlBigInt . fromIntegral . permsToInt
     in Cql.CqlUdt [("self", f (p ^. self)), ("copy", f (p ^. copy))]

  fromCql (Cql.CqlUdt p) = do
    let f = intToPerms . fromIntegral :: Int64 -> Set.Set Perm
    s <- Err.note "missing 'self' permissions" ("self" `lookup` p) >>= Cql.fromCql
    d <- Err.note "missing 'copy' permissions" ("copy" `lookup` p) >>= Cql.fromCql
    r <- Err.note "invalid permissions" (newPermissions (f s) (f d))
    pure r
  fromCql _ = fail "permissions: udt expected"
