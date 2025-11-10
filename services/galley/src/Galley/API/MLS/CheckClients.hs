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

module Galley.API.MLS.CheckClients (checkClients) where

import Control.Comonad
import Control.Error.Util (hush)
import Control.Lens
import Control.Lens.Extras (is)
import Control.Monad.Codensity
import Data.Default
import Data.Id
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Data.Tuple.Extra
import Galley.API.Action
import Galley.API.Error
import Galley.API.MLS.Commit.Core
import Galley.API.MLS.Conversation
import Galley.API.MLS.IncomingMessage
import Galley.API.MLS.One2One
import Galley.API.MLS.Proposal
import Galley.API.MLS.Util
import Galley.API.Util
import Galley.Effects
import Galley.Effects.ProposalStore
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Resource (Resource)
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.LeaveReason
import Wire.API.Federation.Error
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Proposal qualified as Proposal
import Wire.API.MLS.SubConversation
import Wire.API.Unreachable
import Wire.API.User.Client
import Wire.ConversationStore
import Wire.ConversationStore.MLS.Types
import Wire.StoredConversation

checkClients ::
  ( Member BrigAPIAccess r,
    Member FederatorAccess r,
    Member (ErrorS MLSClientMismatch) r,
    Member (ErrorS MLSIdentityMismatch) r,
    Member (Error MLSProtocolError) r
  ) =>
  Local ConvOrSubConv ->
  CipherSuiteTag ->
  ClientMap (LeafIndex, Maybe KeyPackage) ->
  Sem r [Qualified UserId]
checkClients lConvOrSub ciphersuite clients = do
  let convOrSub = tUnqualified lConvOrSub
      cm = convOrSub.members
  fmap catMaybes . forM (Map.assocs clients) $
    \(qtarget, newclients) -> do
      -- get list of mls clients from Brig (local or remote)
      mClientData <-
        fmap mkClientData . hush
          <$> runError @FederationError (getClientInfo lConvOrSub qtarget ciphersuite)
      unreachable <- case (mClientData, Map.lookup qtarget cm) of
        -- user is already present, skip check in this case
        (_, Just existingClients) -> do
          -- make sure none of the new clients already exist in the group
          when
            ( any
                (`Map.member` existingClients)
                (Map.keys newclients)
            )
            $ throw
              (mlsProtocolError "Cannot add a client that is already part of the group")
          pure False
        (Nothing, Nothing) -> pure True
        (Just clientData, Nothing) -> do
          -- final set of clients in the conversation
          let clients =
                Map.keysSet
                  ( fmap fst newclients
                      <> Map.findWithDefault mempty qtarget cm
                  )

          -- We check the following condition:
          --   allMLSClients ⊆ clients ⊆ allClients
          -- i.e.
          -- - if a client has at least 1 key package, it has to be added
          -- - if a client is being added, it has to still exist
          --
          -- The reason why we can't simply check that clients == allMLSClients is
          -- that a client with no remaining key packages might be added by a user
          -- who just fetched its last key package.
          unless
            ( Set.isSubsetOf clientData.allMLSClients clients
                && Set.isSubsetOf clients clientData.allClients
            )
            $
            -- FUTUREWORK: turn this error into a proper response
            throwS @'MLSClientMismatch

          pure False

      -- Check that new leaf nodes are using the registered signature keys.
      for_ mClientData $ \clientData ->
        for_ (Map.assocs newclients) $ \(cid, (_, mKp)) ->
          checkSignatureKey (fmap (.leafNode) mKp) (Map.lookup cid clientData.infoMap)

      pure $ guard unreachable $> qtarget

data ClientData = ClientData
  { allClients :: Set (ClientId),
    allMLSClients :: Set (ClientId),
    infoMap :: Map ClientId ByteString
  }

mkClientData :: Set ClientInfo -> ClientData
mkClientData clientInfo =
  ClientData
    { allClients = Set.map (.clientId) clientInfo,
      allMLSClients =
        Set.map
          (.clientId)
          (Set.filter (.hasKeyPackages) clientInfo),
      infoMap =
        Map.fromList
          [ (info.clientId, key)
            | info <- toList clientInfo,
              key <- toList info.mlsSignatureKey
          ]
    }
