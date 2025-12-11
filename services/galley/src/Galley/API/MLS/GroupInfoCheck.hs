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

module Galley.API.MLS.GroupInfoCheck
  ( checkGroupState,
    GroupInfoMismatch (..),
  )
where

import Control.Lens (view)
import Data.Bifunctor
import Data.Id
import Galley.API.Teams.Features.Get
import Galley.Effects
import Galley.Options
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.NonDet
import Wire.API.Conversation hiding (Member)
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.MLS.Credential
import Wire.API.MLS.Extension
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.RatchetTree
import Wire.API.MLS.Serialisation
import Wire.API.Team.Feature
import Wire.ConversationStore
import Wire.ConversationStore.MLS.Types

data GroupInfoMismatch = GroupInfoMismatch
  {clients :: [(Int, ClientIdentity)]}
  deriving (Show)

checkGroupState ::
  forall r.
  ( Member (Error GroupInfoMismatch) r,
    Member (Input Opts) r,
    Member (Error MLSProtocolError) r,
    Member TeamFeatureStore r,
    Member ConversationStore r
  ) =>
  ConvOrSubConv ->
  IndexMap ->
  GroupInfo ->
  Sem r ()
checkGroupState convOrSub newLeaves groupInfo = do
  check <- isGroupInfoCheckEnabled convOrSub.conv.mcMetadata.cnvmTeam
  case (check, groupStateMismatch newLeaves groupInfo) of
    (True, Left e) -> throw (mlsProtocolError e)
    (True, Right (Just mismatch)) -> do
      existingMismatch <- existingGroupStateMismatch convOrSub
      when (isNothing existingMismatch) $ throw mismatch
    _ -> pure ()

groupStateMismatch :: IndexMap -> GroupInfo -> Either Text (Maybe GroupInfoMismatch)
groupStateMismatch leaves groupInfo = do
  trees <-
    first
      (const "Could not parse ratchet tree extension in GroupInfo")
      $ findExtension groupInfo.tbs.extensions
  tree :: RatchetTree <- case trees of
    (tree : _) -> pure tree
    _ -> Left "No ratchet tree extension found in GroupInfo"
  giLeaves <- imFromList <$> traverse (traverse getIdentity) (ratchetTreeLeaves tree)
  pure $ guard (leaves /= giLeaves) $> GroupInfoMismatch (imAssocs leaves)
  where
    getIdentity :: LeafNode -> Either Text ClientIdentity
    getIdentity leaf = fst <$> credentialIdentityAndKey leaf.credential

existingGroupStateMismatch ::
  (Member ConversationStore r) =>
  ConvOrSubConv ->
  Sem r (Maybe GroupInfoMismatch)
existingGroupStateMismatch convOrSub =
  fmap join . runErrorS @MLSMissingGroupInfo $
    do
      groupInfoData <- getConvOrSubGroupInfo convOrSub.id >>= noteS @MLSMissingGroupInfo
      groupInfo <-
        either (\_ -> throwS @MLSMissingGroupInfo) pure $
          decodeMLS' (unGroupInfoData groupInfoData)
      case groupStateMismatch convOrSub.indexMap groupInfo of
        Left _ -> throwS @MLSMissingGroupInfo
        Right m -> pure m

isGroupInfoCheckEnabled ::
  ( Member TeamFeatureStore r,
    Member (Input Opts) r
  ) =>
  Maybe TeamId ->
  Sem r Bool
isGroupInfoCheckEnabled Nothing = pure False
isGroupInfoCheckEnabled (Just tid) = fmap isJust . runNonDetMaybe $ do
  global <- inputs (view $ settings . checkGroupInfo)
  guard (global == Just True)
  mls <- getFeatureForTeam @MLSConfig tid
  guard (getAny mls.config.mlsGroupInfoDiagnostics)
