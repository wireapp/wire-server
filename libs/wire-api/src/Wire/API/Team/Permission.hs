{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  ( -- * Permissions
    Permissions (..),
    self,
    copy,
    newPermissions,
    fullPermissions,
    noPermissions,
    serviceWhitelistPermissions,

    -- * Permissions
    Perm (..),
    permsToInt,
    intToPerms,
    permToInt,
    intToPerm,

    -- * Swagger
    modelPermissions,
  )
where

import qualified Cassandra as Cql
import qualified Control.Error.Util as Err
import Control.Lens (makeLenses, (^.))
import Data.Aeson
import Data.Bits (testBit, (.|.))
import Data.Json.Util
import qualified Data.Set as Set
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- Permissions

data Permissions = Permissions
  { _self :: Set Perm,
    _copy :: Set Perm
  }
  deriving stock (Eq, Ord, Show, Generic)

modelPermissions :: Doc.Model
modelPermissions = Doc.defineModel "Permissions" $ do
  Doc.description
    "Permissions constrain possible member actions.\
    \ The currently defined permissions can be found in: \
    \ https://github.com/wireapp/wire-server/blob/develop/libs/galley-types/src/Galley/Types/Teams.hs#L247"
  Doc.property "self" (Doc.int64 $ Doc.min 0 . Doc.max 0x7FFFFFFFFFFFFFFF) $
    Doc.description "The permissions bitmask which applies to this user"
  Doc.property "copy" (Doc.int64 $ Doc.min 0 . Doc.max 0x7FFFFFFFFFFFFFFF) $
    Doc.description "The permissions bitmask which this user can assign to others"

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

instance Arbitrary Permissions where
  arbitrary =
    maybe (error "instance Arbitrary Permissions") pure =<< do
      selfperms <- arbitrary
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
      DoNotUseDeprecatedAddRemoveConvMember,
      SetTeamData
    ]

--------------------------------------------------------------------------------
-- Perm

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
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving (Arbitrary) via (GenericUniform Perm)

permsToInt :: Set Perm -> Word64
permsToInt = Set.foldr' (\p n -> n .|. permToInt p) 0

intToPerms :: Word64 -> Set Perm
intToPerms n =
  let perms = [2 ^ i | i <- [0 .. 62], n `testBit` i]
   in Set.fromList (mapMaybe intToPerm perms)

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

makeLenses ''Permissions

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
