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

module Galley.API.MLS.Removal where

import Control.Comonad
import Control.Lens (view)
import Data.Id
import Data.Qualified
import Data.Time
import Galley.API.Error
import Galley.API.MLS.Propagate
import Galley.Data.Conversation.Types hiding (Conversation)
import qualified Galley.Data.Conversation.Types as Data
import Galley.Effects
import Galley.Effects.ProposalStore
import Galley.Env
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Conversation.Protocol
import Wire.API.MLS.Credential
import Wire.API.MLS.Keys
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation

removeUser ::
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
  Qualified UserId ->
  Sem r ()
removeUser lc qusr = do
  case Data.convProtocol (tUnqualified lc) of
    ProtocolProteus -> pure ()
    ProtocolMLS meta -> do
      keyPair <- mlsKeyPair_ed25519 <$> (inputs (view mlsKeys) <*> pure RemovalPurpose)
      (secKey, pubKey) <- note (InternalErrorWithDescription "backend removal key missing") $ keyPair
      for_ (getConvMemberMLSClients lc qusr) $ \cpks ->
        for_ cpks $ \(_client, kpref) -> do
          let proposal = mkRemoveProposal kpref
              msg = mkSignedMessage secKey pubKey (cnvmlsGroupId meta) (cnvmlsEpoch meta) (ProposalMessage proposal)
              msgEncoded = encodeMLS' msg
          storeProposal
            (cnvmlsGroupId meta)
            (cnvmlsEpoch meta)
            (proposalRef (cnvmlsCipherSuite meta) proposal)
            proposal
          propagateMessage qusr lc Nothing msgEncoded
