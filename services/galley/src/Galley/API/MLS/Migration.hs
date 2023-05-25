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

module Galley.API.MLS.Migration (checkMigrationCriteria) where

import Data.Qualified
import Data.Time
import Galley.API.MLS.Types
import Galley.Effects.BrigAccess
import Galley.Effects.ClientStore
import Galley.Effects.FederatorAccess
import Galley.Types.Clients
import Galley.Types.Conversations.Members
import Imports
import Polysemy
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Team.Feature
import Wire.API.UserMap

-- | Similar to @Ap f All@, but short-circuiting.
--
-- For example:
-- @
-- ApAll (pure False) <> ApAll (putStrLn "hi" $> True)
-- @
-- does not print anything.
newtype ApAll f = ApAll {unApAll :: f Bool}

instance Monad f => Semigroup (ApAll f) where
  ApAll a <> ApAll b = ApAll $ a >>= \x -> if x then b else pure False

instance Monad f => Monoid (ApAll f) where
  mempty = ApAll (pure True)

checkMigrationCriteria ::
  ( Member BrigAccess r,
    Member ClientStore r,
    Member FederatorAccess r
  ) =>
  UTCTime ->
  MLSConversation ->
  WithStatus MlsMigrationConfig ->
  Sem r Bool
checkMigrationCriteria now conv ws
  | wsStatus ws == FeatureStatusDisabled = pure False
  | afterDeadline = pure True
  | otherwise =
      unApAll $ mconcat [userThresholdOK, clientThresholdOK]
  where
    mig = wsConfig ws
    afterDeadline = maybe False (now >=) mig.finaliseRegardlessAfter

    numUsers = length conv.mcLocalMembers + length conv.mcRemoteMembers
    numMLSUsers = length conv.mcMembers

    userPercentage = (numMLSUsers * 100) `div` max 1 numUsers
    userThresholdOK = ApAll . pure $ maybe True (userPercentage >=) mig.usersThreshold

    mapLength :: Map k (Set a) -> Int
    mapLength = getSum . foldMap (Sum . length)

    clientThresholdOK = ApAll $ do
      numLocalClients <- do
        let users = map lmId (conv.mcLocalMembers)
        i <- useIntraClientListing
        cs <- if i then fromUserClients <$> lookupClients users else getClients users
        pure (mapLength (toMap cs))
      numRemoteClients <- fmap (sum . map tUnqualified)
        . runFederatedConcurrently (map rmId conv.mcRemoteMembers)
        $ \ruids -> do
          cs <- fedClient @'Brig @"get-user-clients" (GetUserClients (tUnqualified ruids))
          pure (mapLength (userMap cs))
      let numClients = numRemoteClients + numLocalClients
          clientPercentage = (length (cmIdentities conv.mcMembers) * 100) `div` max 1 numClients
      pure $ maybe True (clientPercentage >=) mig.clientsThreshold
