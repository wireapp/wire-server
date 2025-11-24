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

module Galley.API.MLS.OutOfSync
  ( checkConversationOutOfSync,
    updateOutOfSyncFlag,
  )
where

import Data.Id
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Galley.API.MLS.CheckClients
import Galley.Effects
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Error.Galley
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.OutOfSync
import Wire.API.MLS.SubConversation
import Wire.ConversationStore
import Wire.ConversationStore.MLS.Types
import Wire.StoredConversation

checkConversationOutOfSync ::
  ( Member ConversationStore r,
    Member BrigAPIAccess r,
    Member FederatorAccess r,
    Member (Error MLSOutOfSyncError) r,
    Member (Input EnableOutOfSyncCheck) r
  ) =>
  Set (Qualified UserId) ->
  Local ConvOrSubConv ->
  CipherSuiteTag ->
  Sem r ()
checkConversationOutOfSync newMembers lConvOrSub ciphersuite = case tUnqualified lConvOrSub of
  SubConv _ _ -> pure ()
  Conv mc -> do
    enabled <- input
    when (enabled == EnableOutOfSyncCheck) $ do
      flag <- isConversationOutOfSync mc.mcId
      when flag $ do
        let outOfSyncUsers0 = getOutOfSyncUsers newMembers (qualifyAs lConvOrSub mc)
        -- check if all users are reachable and have usable key packages
        outOfSyncUsers <- filterM (checkOutOfSyncUser lConvOrSub ciphersuite) (toList outOfSyncUsers0)
        unless (null outOfSyncUsers) $
          throw (MLSOutOfSyncError outOfSyncUsers)

checkOutOfSyncUser ::
  ( Member BrigAPIAccess r,
    Member FederatorAccess r
  ) =>
  Local x ->
  CipherSuiteTag ->
  Qualified UserId ->
  Sem r Bool
checkOutOfSyncUser loc ciphersuite qtarget =
  getClientData loc ciphersuite qtarget >>= \case
    Nothing -> pure False
    Just cdata -> pure (not (Set.null cdata.allMLSClients))

updateOutOfSyncFlag ::
  (Member ConversationStore r) =>
  ClientIdentity ->
  Local ConvOrSubConv ->
  Sem r ()
updateOutOfSyncFlag sender lconv = case tUnqualified lconv of
  SubConv _ _ -> pure ()
  Conv c -> do
    let newMembers = Set.singleton (cidQualifiedUser sender)
    when (Set.null (getOutOfSyncUsers newMembers (qualifyAs lconv c))) $
      setConversationOutOfSync c.mcId False

getOutOfSyncUsers :: Set (Qualified UserId) -> Local MLSConversation -> Set (Qualified UserId)
getOutOfSyncUsers newMembers lconv =
  let conv = tUnqualified lconv
      convMembers =
        Set.fromList $
          map (tUntagged . qualifyAs lconv . (.id_)) conv.mcLocalMembers
            <> map (tUntagged . (.id_)) conv.mcRemoteMembers
      groupMembers = Map.keysSet conv.mcMembers <> newMembers
   in Set.difference convMembers groupMembers
