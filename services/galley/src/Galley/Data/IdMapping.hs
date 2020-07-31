-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Data.IdMapping
  ( getIdMapping,
    insertIdMapping,
  )
where

import Cassandra (Consistency (Quorum), MonadClient, params, query1, retry, write, x1, x5)
import Data.Id (Id, Mapped)
import Data.IdMapping (IdMapping (IdMapping, _imMappedId, _imQualifiedId))
import Data.Qualified (Qualified (Qualified, _qDomain, _qLocalPart))
import Galley.Data.Instances ()
import qualified Galley.Data.Queries as Cql
import Imports

-- | Only a single namespace/table is used for for potentially multiple different types of
-- mapped IDs.
getIdMapping :: MonadClient m => Id (Mapped a) -> m (Maybe (IdMapping a))
getIdMapping mappedId =
  fmap toIdMapping <$> do
    retry x1 $ query1 Cql.selectIdMapping (params Quorum (Identity mappedId))
  where
    toIdMapping (remoteId, domain) =
      IdMapping mappedId (Qualified remoteId domain)

-- | Only a single namespace/table is used for for potentially multiple different types of
-- mapped IDs.
insertIdMapping :: MonadClient m => IdMapping a -> m ()
insertIdMapping idMapping = do
  retry x5 $ write Cql.insertIdMapping (params Quorum (mappedId, remoteId, domain))
  where
    mappedId = _imMappedId idMapping
    remoteId = _qLocalPart (_imQualifiedId idMapping)
    domain = _qDomain (_imQualifiedId idMapping)
