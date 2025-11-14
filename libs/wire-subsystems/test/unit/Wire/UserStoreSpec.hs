{-# OPTIONS -Wno-ambiguous-fields #-}

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

module Wire.UserStoreSpec (spec) where

import Data.Default
import Imports
import Polysemy.State
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.User
import Wire.MiniBackend
import Wire.StoredUser
import Wire.UserStore

spec :: Spec
spec = do
  describe "mkUserFromStored" $ do
    prop "user identity" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in if not storedUser.activated
            then user.userIdentity === Nothing
            else
              (emailIdentity =<< user.userIdentity) === storedUser.email
                .&&. (ssoIdentity =<< user.userIdentity) === storedUser.ssoId

    prop "user deleted" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in userDeleted user === (storedUser.status == Just Deleted)

    prop "user expires" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in if storedUser.status == Just Ephemeral
            then user.userExpire === storedUser.expires
            else user.userExpire === Nothing

    prop "user locale" $ \domain defaultLocale storedUser ->
      let user = mkUserFromStored domain defaultLocale storedUser
       in if (isJust storedUser.language)
            then user.userLocale === Locale (fromJust storedUser.language) storedUser.country
            else user.userLocale === defaultLocale

  describe "UserStore effect" $ do
    prop "user self email deleted" $ \user1 user2' email2 config ->
      let user2 = user2' {email = Just email2} :: StoredUser
          localBackend = def {users = [user1, user2]}
          result =
            runNoFederationStack localBackend mempty config $ do
              deleteEmail (user1.id)
              gets users
       in result === [user1 {email = Nothing}, user2]
    prop "update unvalidated email" $ \user1 user2 email1 config ->
      let updatedUser1 = user1 {emailUnvalidated = Just email1} :: StoredUser
          localBackend = def {users = [user1, user2]}
          result =
            runNoFederationStack localBackend mempty config $ do
              updateEmailUnvalidated (user1.id) email1
              gets users
       in result === [updatedUser1, user2]
