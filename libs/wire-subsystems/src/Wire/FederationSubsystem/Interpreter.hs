-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.FederationSubsystem.Interpreter
  ( runFederationSubsystem,
  )
where

import Data.Qualified
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Component (Component (..))
import Wire.API.Conversation.Protocol (ProtocolTag)
import Wire.API.Error.Galley (NonFederatingBackends (..), UnreachableBackends (..))
import Wire.API.Federation.API (fedClient)
import Wire.API.Federation.API.Brig (DomainSet (..))
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.API.FederationStatus (FederationStatus (..), RemoteDomains (..))
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.FederationAPIAccess qualified as E
import Wire.FederationSubsystem
import Wire.FederationSubsystem.Internals (firstMissingConnectionOrFullyConnected)

runFederationSubsystem ::
  ( Member (Error FederationError) r,
    Member (Error UnreachableBackends) r,
    Member (Error NonFederatingBackends) r,
    Member (FederationAPIAccess FederatorClient) r
  ) =>
  Maybe [ProtocolTag] ->
  Sem (FederationSubsystem : r) a ->
  Sem r a
runFederationSubsystem mAllowedProtos = interpret $ \case
  EnforceFederationProtocol proto domains ->
    unless (null domains) $ do
      unless (maybe True (elem proto) mAllowedProtos) $
        throw FederationDisabledForProtocol
  CheckFederationStatus req -> do
    status <- getFederationStatusImpl req
    case status of
      FullyConnected -> pure ()
      NotConnectedDomains dom1 dom2 -> throw (NonFederatingBackends dom1 dom2)
  GetFederationStatus req -> getFederationStatusImpl req

getFederationStatusImpl ::
  ( Member (Error UnreachableBackends) r,
    Member (FederationAPIAccess FederatorClient) r
  ) =>
  RemoteDomains ->
  Sem r FederationStatus
getFederationStatusImpl req = do
  fmap firstMissingConnectionOrFullyConnected
    . (ensureNoUnreachableBackends =<<)
    $ E.runFederatedConcurrentlyEither
      (Set.toList req.rdDomains)
      ( \qds ->
          fedClient @'Brig @"get-not-fully-connected-backends"
            (DomainSet . Set.map tDomain $ void qds `Set.delete` req.rdDomains)
      )
