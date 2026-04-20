{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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

module Wire.AppSubsystem.InterpreterSpec (spec) where

import Data.Default
import Data.Domain (Domain (..))
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Map qualified as Map
import Data.Misc (plainTextPassword6Unsafe)
import Data.Qualified
import Data.Range
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Team.Member
import Wire.API.Team.Role
import Wire.API.User
import Wire.AppSubsystem
import Wire.MiniBackend
import Wire.MockInterpreters.HashPassword (hashPassword)
import Wire.StoredUser
import Wire.UserSubsystem.Interpreter

spec :: Spec
spec = describe "AppSubsystem.Interpreter" $ do
  describe "createApp" $ do
    prop "stores the team name as the app's author" $
      \(owner :: StoredUser) (tid :: TeamId) (config :: UserSubsystemConfig) (newAppBase :: NewApp) ->
        -- Use a fixed password so we can store the matching hash.
        let pw = plainTextPassword6Unsafe "TestPassword123!"
            newApp :: NewApp
            newApp = newAppBase {password = pw}
            ownerMember = mkTeamMember owner.id (rolePermissions RoleAdmin) Nothing UserLegalHoldDisabled
            teamMap = Map.singleton tid [ownerMember]
            teamName = "Acme Corp"
            backend =
              def
                { users = [owner],
                  userPasswords = Map.singleton owner.id (hashPassword pw),
                  teamNames = Map.singleton tid teamName
                }
            authUser = toLocalUnsafe (Domain "localdomain") owner.id
            result =
              runNoFederationStack backend teamMap config $ do
                createdApp <- createApp authUser tid newApp
                pure $ fmap (.author) createdApp.user.profileApp
         in result === Just (unsafeRange teamName)
