{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
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
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- The above pragma is to ignore unused `genSingletons` definitions of promoted
-- constructors

module Wire.API.Team.Permission
  ( -- * Permissions
    Permissions (..),
    newPermissions,
    fullPermissions,
    noPermissions,
    serviceWhitelistPermissions,

    -- * Permissions
    Perm (..),
    SPerm (..),
    permsToInt,
    intToPerms,
    permToInt,
    intToPerm,
  )
where

import Cassandra qualified as Cql
import Control.Error.Util qualified as Err
import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Bits (testBit, (.|.))
import Data.OpenApi qualified as S
import Data.Schema
import Data.Set qualified as Set
import Data.Singletons.Base.TH
import Imports
import Test.QuickCheck (oneof)
import Wire.API.Util.Aeson (CustomEncoded (..))
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- Permissions

data Permissions = Permissions
  { -- | User's permissions
    self :: Set Perm,
    -- | Permissions this user is allowed to grant others
    copy :: Set Perm
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema Permissions)

permissionsSchema :: ValueSchema NamedSwaggerDoc Permissions
permissionsSchema =
  objectWithDocModifier "Permissions" (description ?~ docs) $
    Permissions
      <$> (permsToInt . self) .= fieldWithDocModifier "self" selfDoc (intToPerms <$> schema)
      <*> (permsToInt . copy) .= fieldWithDocModifier "copy" copyDoc (intToPerms <$> schema)
  where
    selfDoc = S.description ?~ "Permissions that the user has"
    copyDoc = S.description ?~ "Permissions that this user is able to grant others"
    docs =
      "This is just a complicated way of representing a team role.  self and copy \
      \always have to contain the same integer, and only the following integers \
      \are allowed: 1025 (partner), 1587 (member), 5951 (admin), 8191 (owner). \
      \Unit tests of the galley-types package in wire-server contain an authoritative \
      \list."

instance ToSchema Permissions where
  schema = withParser permissionsSchema $ \(Permissions s d) ->
    case newPermissions s d of
      Nothing -> fail "invalid permissions"
      Just ps -> pure ps

instance Arbitrary Permissions where
  arbitrary =
    maybe (error "instance Arbitrary Permissions") pure =<< do
      selfperms <- oneof $ map (pure . intToPerms) [1025, 1587, 5951, 8191]
      copyperms <- Set.intersection selfperms <$> arbitrary
      pure $ newPermissions selfperms copyperms

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
      AddRemoveConvMember,
      SetTeamData
    ]

--------------------------------------------------------------------------------
-- Perm

-- | Team-level permission.  Analog to conversation-level 'Action'.
data Perm
  = CreateConversation
  | -- NOTE: This may get overruled by conv level checks in case those are more restrictive
    -- We currently cannot get rid of this team-level permission in favor of the conv-level action
    -- because it is used for e.g. for the team role 'RoleExternalPartner'
    DeleteConversation
  | AddTeamMember
  | RemoveTeamMember
  | -- NOTE: This may get overruled by conv level checks in case those are more restrictive
    -- We currently cannot get rid of this team-level permission in favor of the conv-level action
    -- because it is used for e.g. for the team role 'RoleExternalPartner'
    AddRemoveConvMember
  | -- NOTE: This may get overruled by conv level checks in case those are more restrictive
    -- We currently cannot get rid of this team-level permission in favor of the conv-level action
    -- because it is used for e.g. for the team role 'RoleExternalPartner'
    ModifyConvName
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
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving (Arbitrary) via (GenericUniform Perm)
  deriving (FromJSON, ToJSON) via (CustomEncoded Perm)

instance S.ToSchema Perm

permsToInt :: Set Perm -> Word64
permsToInt = Set.foldr' (\p n -> n .|. permToInt p) 0

intToPerms :: Word64 -> Set Perm
intToPerms n =
  let perms = [2 ^ i | i <- [0 .. 62], n `testBit` i]
   in Set.fromList (mapMaybe intToPerm perms)

permToInt :: Perm -> Word64
permToInt CreateConversation = 0x0001
permToInt DeleteConversation = 0x0002
permToInt AddTeamMember = 0x0004
permToInt RemoveTeamMember = 0x0008
permToInt AddRemoveConvMember = 0x0010
permToInt ModifyConvName = 0x0020
permToInt GetBilling = 0x0040
permToInt SetBilling = 0x0080
permToInt SetTeamData = 0x0100
permToInt GetMemberPermissions = 0x0200
permToInt GetTeamConversations = 0x0400
permToInt DeleteTeam = 0x0800
permToInt SetMemberPermissions = 0x1000

intToPerm :: Word64 -> Maybe Perm
intToPerm 0x0001 = Just CreateConversation
intToPerm 0x0002 = Just DeleteConversation
intToPerm 0x0004 = Just AddTeamMember
intToPerm 0x0008 = Just RemoveTeamMember
intToPerm 0x0010 = Just AddRemoveConvMember
intToPerm 0x0020 = Just ModifyConvName
intToPerm 0x0040 = Just GetBilling
intToPerm 0x0080 = Just SetBilling
intToPerm 0x0100 = Just SetTeamData
intToPerm 0x0200 = Just GetMemberPermissions
intToPerm 0x0400 = Just GetTeamConversations
intToPerm 0x0800 = Just DeleteTeam
intToPerm 0x1000 = Just SetMemberPermissions
intToPerm _ = Nothing

instance Cql.Cql Permissions where
  ctype = Cql.Tagged $ Cql.UdtColumn "permissions" [("self", Cql.BigIntColumn), ("copy", Cql.BigIntColumn)]

  toCql p =
    let f = Cql.CqlBigInt . fromIntegral . permsToInt
     in Cql.CqlUdt [("self", f p.self), ("copy", f p.copy)]

  fromCql (Cql.CqlUdt p) = do
    let f = intToPerms . fromIntegral :: Int64 -> Set.Set Perm
    s <- Err.note "missing 'self' permissions" ("self" `lookup` p) >>= Cql.fromCql
    d <- Err.note "missing 'copy' permissions" ("copy" `lookup` p) >>= Cql.fromCql
    Err.note "invalid permissions" (newPermissions (f s) (f d))
  fromCql _ = Left "permissions: udt expected"

$(genSingletons [''Perm])

$(promoteShowInstances [''Perm])
