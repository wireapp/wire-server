-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.API.MLS.Migration where

import Brig.Types.Intra
import Data.Qualified
import Data.Set qualified as Set
import Data.Time
import Galley.Effects.FederatorAccess
import Imports
import Polysemy
import Wire.API.Federation.API
import Wire.API.Team.Feature
import Wire.API.User
import Wire.BrigAPIAccess
import Wire.ConversationStore.MLS.Types
import Wire.StoredConversation

-- | Similar to @Ap f All@, but short-circuiting.
--
-- For example:
-- @
-- ApAll (pure False) <> ApAll (putStrLn "hi" $> True)
-- @
-- does not print anything.
newtype ApAll f = ApAll {unApAll :: f Bool}

instance (Monad f) => Semigroup (ApAll f) where
  ApAll a <> ApAll b = ApAll $ a >>= \x -> if x then b else pure False

instance (Monad f) => Monoid (ApAll f) where
  mempty = ApAll (pure True)

checkMigrationCriteria ::
  ( Member BrigAPIAccess r,
    Member FederatorAccess r
  ) =>
  UTCTime ->
  MLSConversation ->
  LockableFeature MlsMigrationConfig ->
  Sem r Bool
checkMigrationCriteria now conv ws
  | ws.status == FeatureStatusDisabled = pure False
  | afterDeadline = pure True
  | otherwise = unApAll $ mconcat [localUsersMigrated, remoteUsersMigrated]
  where
    afterDeadline = maybe False (now >=) ws.config.finaliseRegardlessAfter

    containsMLS = Set.member BaseProtocolMLSTag

    localUsersMigrated = ApAll $ do
      localProfiles <-
        getUsers (map (.id_) conv.mcLocalMembers)
      pure $ all (containsMLS . userSupportedProtocols) localProfiles

    remoteUsersMigrated = ApAll $ do
      remoteProfiles <- fmap (foldMap tUnqualified)
        . runFederatedConcurrently (map (.id_) conv.mcRemoteMembers)
        $ \ruids ->
          fedClient @'Brig @"get-users-by-ids" (tUnqualified ruids)
      pure $ all (containsMLS . profileSupportedProtocols) remoteProfiles
