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

module Galley.API.MLS.GroupInfoCheck (checkGroupState) where

import Control.Lens (view)
import Data.Id
import Galley.API.Teams.Features.Get
import Galley.Effects
import Galley.Options
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.NonDet
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.MLS.Credential
import Wire.API.MLS.Extension
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.RatchetTree
import Wire.API.Team.Feature
import Wire.ConversationStore.MLS.Types

checkGroupState ::
  forall r.
  ( Member (ErrorS MLSGroupInfoMismatch) r,
    Member (Input Opts) r,
    Member (Error MLSProtocolError) r,
    Member TeamFeatureStore r
  ) =>
  IndexMap ->
  GroupInfo ->
  Sem r ()
checkGroupState leaves groupInfo = do
  check <- isGroupInfoCheckEnabled Nothing
  when check $ do
    trees <-
      either
        (\_ -> throw (mlsProtocolError "Could not parse ratchet tree extension in GroupInfo"))
        pure
        $ findExtension groupInfo.tbs.extensions
    tree :: RatchetTree <- case trees of
      (tree : _) -> pure tree
      _ -> throw $ mlsProtocolError "No ratchet tree extension found in GroupInfo"
    giLeaves <- imFromList <$> traverse (traverse getIdentity) (ratchetTreeLeaves tree)
    when (leaves /= giLeaves) $ do
      throwS @MLSGroupInfoMismatch
  where
    getIdentity :: LeafNode -> Sem r ClientIdentity
    getIdentity leaf = case credentialIdentityAndKey leaf.credential of
      Left e -> throw (mlsProtocolError e)
      Right (cid, _) -> pure cid

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
  ops <- getFeatureForTeam @AllowedGlobalOperationsConfig tid
  guard ops.config.mlsGroupInfoDiagnostics
