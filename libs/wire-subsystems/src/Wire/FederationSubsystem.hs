{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.FederationSubsystem where

import Data.Qualified
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Conversation.Protocol (ProtocolTag)
import Wire.API.Error.Galley (UnreachableBackends (..))
import Wire.API.FederationStatus

data FederationSubsystem m a where
  EnforceFederationProtocol :: ProtocolTag -> [Remote ()] -> FederationSubsystem m ()
  CheckFederationStatus :: RemoteDomains -> FederationSubsystem m ()
  GetFederationStatus :: RemoteDomains -> FederationSubsystem m FederationStatus

makeSem ''FederationSubsystem

ensureNoUnreachableBackends ::
  (Member (Error UnreachableBackends) r) =>
  [Either (Remote e, b) a] ->
  Sem r [a]
ensureNoUnreachableBackends results = do
  let (errors, values) = partitionEithers results
  unless (null errors) $
    throw (UnreachableBackends (map (tDomain . fst) errors))
  pure values
