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
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.SubConversation
import Wire.ConversationStore
import Wire.ConversationStore.MLS.Types
import Wire.StoredConversation

checkConversationOutOfSync ::
  ( Member ConversationStore r,
    Member BrigAPIAccess r,
    Member FederatorAccess r
  ) =>
  Set (Qualified UserId) ->
  Local ConvOrSubConv ->
  CipherSuiteTag ->
  Sem r Bool
checkConversationOutOfSync newMembers lConvOrSub ciphersuite = case tUnqualified lConvOrSub of
  SubConv _ _ -> pure False
  Conv mc -> do
    flag <- isConversationOutOfSync mc.mcId
    if flag
      then do
        let outOfSyncUsers = getOutOfSyncUsers newMembers (qualifyAs lConvOrSub mc)
        -- check if all users are reachable and have usable key packages
        getAny
          <$> foldMap
            ( fmap Any
                . checkOutOfSyncUser lConvOrSub ciphersuite
            )
            outOfSyncUsers
      else
        pure False

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
