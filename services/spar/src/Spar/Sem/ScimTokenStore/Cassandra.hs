{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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

module Spar.Sem.ScimTokenStore.Cassandra where

import Cassandra
import Imports
import Polysemy
import qualified Spar.Data as Data
import Spar.Sem.ScimTokenStore

scimTokenStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (ScimTokenStore ': r) a ->
  Sem r a
scimTokenStoreToCassandra =
  interpret $
    embed @m . \case
      Insert st sti -> Data.insertScimToken st sti
      Lookup st -> Data.lookupScimToken st
      GetByTeam tid -> Data.getScimTokens tid
      Delete tid ur -> Data.deleteScimToken tid ur
      DeleteByTeam tid -> Data.deleteTeamScimTokens tid
