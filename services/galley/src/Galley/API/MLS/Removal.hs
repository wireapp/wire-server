-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.API.MLS.Removal
  ( removeClientsWithClientMap,
    removeClient,
    removeUserWithClientMap,
    removeUser,
  )
where

import Control.Comonad
import Data.Id
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import Data.Time
import Galley.API.Error
import Galley.API.MLS.Keys (getMLSRemovalKey)
import Galley.API.MLS.Propagate
import Galley.API.MLS.Types
import qualified Galley.Data.Conversation.Types as Data
import Galley.Effects
import Galley.Effects.MemberStore
import Galley.Effects.ProposalStore
import Galley.Env
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import qualified System.Logger as Log
import Wire.API.Conversation.Protocol
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation

-- | Send remove proposals for a set of clients to clients in the ClientMap.
removeClientsWithClientMap ::
  ( Members
      '[ Input UTCTime,
         TinyLog,
         ExternalAccess,
         FederatorAccess,
         GundeckAccess,
         Error InternalError,
         ProposalStore,
         Input Env
       ]
      r
  ) =>
  Local Data.Conversation ->
  Set (ClientId, KeyPackageRef) ->
  ClientMap ->
  Qualified UserId ->
  Sem r ()
removeClientsWithClientMap lc cs cm qusr = do
  case Data.convProtocol (tUnqualified lc) of
    ProtocolProteus -> pure ()
    ProtocolMLS meta -> do
      mKeyPair <- getMLSRemovalKey
      case mKeyPair of
        Nothing -> do
          warn $ Log.msg ("No backend removal key is configured (See 'mlsPrivateKeyPaths' in galley's config). Not able to remove client from MLS conversation." :: Text)
        Just (secKey, pubKey) -> do
          for_ cs $ \(_client, kpref) -> do
            let proposal = mkRemoveProposal kpref
                msg = mkSignedMessage secKey pubKey (cnvmlsGroupId meta) (cnvmlsEpoch meta) (ProposalMessage proposal)
                msgEncoded = encodeMLS' msg
            storeProposal
              (cnvmlsGroupId meta)
              (cnvmlsEpoch meta)
              (proposalRef (cnvmlsCipherSuite meta) proposal)
              proposal
            propagateMessage qusr lc cm Nothing msgEncoded

-- | Send remove proposals for a single client of a user to the local conversation.
removeClient ::
  ( Members
      '[ Error InternalError,
         ExternalAccess,
         FederatorAccess,
         GundeckAccess,
         Input Env,
         Input UTCTime,
         MemberStore,
         ProposalStore,
         TinyLog
       ]
      r
  ) =>
  Local Data.Conversation ->
  Qualified UserId ->
  ClientId ->
  Sem r ()
removeClient lc qusr cid = do
  cm <- lookupMLSClients (fmap Data.convId lc)
  let cidAndKP = Set.filter ((==) cid . fst) $ Map.findWithDefault mempty qusr cm
  removeClientsWithClientMap lc cidAndKP cm qusr

-- | Send remove proposals for all clients of the user to clients in the ClientMap.
--
-- All clients of the user have to be contained in the ClientMap.
removeUserWithClientMap ::
  ( Members
      '[ Input UTCTime,
         TinyLog,
         ExternalAccess,
         FederatorAccess,
         GundeckAccess,
         Error InternalError,
         ProposalStore,
         Input Env
       ]
      r
  ) =>
  Local Data.Conversation ->
  ClientMap ->
  Qualified UserId ->
  Sem r ()
removeUserWithClientMap lc cm qusr =
  removeClientsWithClientMap lc (Map.findWithDefault mempty qusr cm) cm qusr

-- | Send remove proposals for all clients of the user to the local conversation.
removeUser ::
  ( Members
      '[ Error InternalError,
         ExternalAccess,
         FederatorAccess,
         GundeckAccess,
         Input Env,
         Input UTCTime,
         MemberStore,
         ProposalStore,
         TinyLog
       ]
      r
  ) =>
  Local Data.Conversation ->
  Qualified UserId ->
  Sem r ()
removeUser lc qusr = do
  cm <- lookupMLSClients (fmap Data.convId lc)
  removeUserWithClientMap lc cm qusr
