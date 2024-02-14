{-# LANGUAGE DeepSubsumption #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Effects.ConnectionStore.Cassandra where

import Brig.Data.Connection
import Brig.Effects.ConnectionStore
import Cassandra
import Data.Range
import Imports
import Polysemy
import Polysemy.Internal.Tactics
import Wire.Sem.Paging.Cassandra

connectionStoreToCassandra ::
  forall r a.
  (Member (Embed Client) r) =>
  Sem (ConnectionStore InternalPaging ': r) a ->
  Sem r a
connectionStoreToCassandra =
  interpretH $
    liftT . embed @Client . \case
      RemoteConnectedUsersPaginated uid mps bounds -> case mps of
        Nothing -> flip mkInternalPage pure =<< lookupRemoteConnectedUsersPaginated uid (fromRange bounds)
        Just ps -> ipNext ps
