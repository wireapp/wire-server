{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

module Wire.ConversationSubsystem.Federation where

import Control.Error (headMay)
import Data.Domain (Domain)
import Data.Qualified
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Component (Component (..))
import Wire.API.Conversation.Protocol (ProtocolTag)
import Wire.API.Error.Galley (NonFederatingBackends (..), UnreachableBackends (..))
import Wire.API.Federation.API (fedClient)
import Wire.API.Federation.API.Brig (DomainSet (..), NonConnectedBackends (..))
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error
import Wire.API.FederationStatus
import Wire.ConversationSubsystem.Types
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.FederationAPIAccess qualified as E

enforceFederationProtocol ::
  ( Member (Error FederationError) r,
    Member (Input ConversationSubsystemConfig) r
  ) =>
  ProtocolTag ->
  [Remote ()] ->
  Sem r ()
enforceFederationProtocol proto domains = do
  unless (null domains) $ do
    mAllowedProtos <- federationProtocols <$> input
    unless (maybe True (elem proto) mAllowedProtos) $
      throw FederationDisabledForProtocol

checkFederationStatus ::
  ( Member (Error UnreachableBackends) r,
    Member (Error NonFederatingBackends) r,
    Member (FederationAPIAccess FederatorClient) r
  ) =>
  RemoteDomains ->
  Sem r ()
checkFederationStatus req = do
  status <- getFederationStatus req
  case status of
    FullyConnected -> pure ()
    NotConnectedDomains dom1 dom2 -> throw (NonFederatingBackends dom1 dom2)

getFederationStatus ::
  ( Member (Error UnreachableBackends) r,
    Member (FederationAPIAccess FederatorClient) r
  ) =>
  RemoteDomains ->
  Sem r FederationStatus
getFederationStatus req = do
  fmap firstConflictOrFullyConnected
    . (ensureNoUnreachableBackends =<<)
    $ E.runFederatedConcurrentlyEither
      (Set.toList req.rdDomains)
      ( \qds ->
          fedClient @'Brig @"get-not-fully-connected-backends"
            (DomainSet . Set.map tDomain $ void qds `Set.delete` req.rdDomains)
      )

-- | "conflict" here means two remote domains that we are connected to
-- but are not connected to each other.
firstConflictOrFullyConnected :: [Remote NonConnectedBackends] -> FederationStatus
firstConflictOrFullyConnected =
  maybe
    FullyConnected
    (uncurry NotConnectedDomains)
    . headMay
    . mapMaybe toMaybeConflict
  where
    toMaybeConflict :: Remote NonConnectedBackends -> Maybe (Domain, Domain)
    toMaybeConflict r =
      headMay (Set.toList (nonConnectedBackends (tUnqualified r))) <&> (tDomain r,)

ensureNoUnreachableBackends ::
  (Member (Error UnreachableBackends) r) =>
  [Either (Remote e, b) a] ->
  Sem r [a]
ensureNoUnreachableBackends results = do
  let (errors, values) = partitionEithers results
  unless (null errors) $
    throw (UnreachableBackends (map (tDomain . fst) errors))
  pure values
