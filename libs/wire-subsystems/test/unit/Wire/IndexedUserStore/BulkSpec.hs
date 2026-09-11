-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.IndexedUserStore.BulkSpec where

import Cassandra.Util (Writetime (Writetime))
import Control.Exception (ErrorCall (..))
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Map qualified as Map
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Data.UUID qualified as UUID
import Imports
import Test.Hspec
import Wire.API.Team.Member.Info (TeamMemberInfo (..))
import Wire.API.Team.Permission (Permissions, fullPermissions, noPermissions)
import Wire.API.Team.Role (Role (..))
import Wire.IndexedUserStore.Bulk.ElasticSearch (mkRoleLookup, rolesFromMemberInfos)
import Wire.UserStore.IndexUser (WithWritetime (..))

spec :: Spec
spec = do
  describe "rolesFromMemberInfos" $ do
    it "keeps members whose permissions map to a role, with their writetime" $ do
      rolesFromMemberInfos [memberInfo uid1 fullPermissions]
        `shouldBe` Map.fromList [(uid1, withWritetime RoleOwner)]

    it "drops members whose permissions map to no role" $ do
      rolesFromMemberInfos [memberInfo uid1 noPermissions] `shouldBe` mempty

  describe "mkRoleLookup" $ do
    it "finds roles across all the teams of a page" $ do
      let lookupRole = mkRoleLookup [Right (roleMap uid1 RoleOwner), Right (roleMap uid2 RoleMember)]
      simplify (lookupRole uid1) `shouldBe` Right (Just (withWritetime RoleOwner))
      simplify (lookupRole uid2) `shouldBe` Right (Just (withWritetime RoleMember))

    -- galley answered 2xx, it just doesn't have a team member entry for this
    -- account: that means "no role", not "error".
    it "treats a member galley does not know about as role-less" $ do
      let lookupRole = mkRoleLookup [Right (roleMap uid1 RoleOwner)]
      simplify (lookupRole uid2) `shouldBe` Right Nothing

    -- galley answered non-2xx: we cannot tell role-less accounts from accounts
    -- whose role we failed to fetch, so the whole page has to fail.
    it "fails every member of the page if the lookup failed for any team" $ do
      let lookupRole =
            mkRoleLookup
              [ Right (roleMap uid1 RoleOwner),
                Left (toException (ErrorCall "galley is down"))
              ]
      lookupRole uid1 `shouldSatisfy` isLeft
      lookupRole uid2 `shouldSatisfy` isLeft

    it "is role-less, not failing, on an empty page" $ do
      simplify (mkRoleLookup [] uid1) `shouldBe` Right Nothing
  where
    uid1, uid2 :: UserId
    uid1 = Id $ UUID.fromWords 1 1 1 1
    uid2 = Id $ UUID.fromWords 2 2 2 2

    writeTime :: UTCTime
    writeTime = UTCTime (fromGregorian 2026 9 11) 0

    memberInfo :: UserId -> Permissions -> TeamMemberInfo
    memberInfo uid perms =
      TeamMemberInfo
        { userId = uid,
          permissions = perms,
          permissionsWriteTime = toUTCTimeMillis writeTime
        }

    withWritetime :: Role -> WithWritetime Role
    withWritetime role = WithWriteTime {value = role, writetime = Writetime writeTime}

    roleMap :: UserId -> Role -> Map UserId (WithWritetime Role)
    roleMap uid role = Map.fromList [(uid, withWritetime role)]

    -- 'SomeException' has no 'Eq', so render it before comparing.
    simplify :: Either SomeException a -> Either String a
    simplify = either (Left . displayException) Right
