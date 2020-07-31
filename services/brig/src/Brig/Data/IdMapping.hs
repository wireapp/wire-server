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

module Brig.Data.IdMapping
  ( getIdMapping,
    insertIdMapping,
  )
where

import Brig.App (AppIO)
import Brig.Data.Instances ()
import Cassandra
import Data.Domain (Domain)
import Data.Id (Id, Mapped, Remote)
import Data.IdMapping (IdMapping (IdMapping, _imMappedId, _imQualifiedId))
import Data.Qualified (Qualified (Qualified, _qDomain, _qLocalPart))
import Imports

-- | Only a single namespace/table is used for for potentially multiple different types of
-- mapped IDs.
getIdMapping :: Id (Mapped a) -> AppIO (Maybe (IdMapping a))
getIdMapping mappedId =
  fmap toIdMapping <$> do
    retry x1 $ query1 idMappingSelect (params Quorum (Identity mappedId))
  where
    toIdMapping (remoteId, domain) =
      IdMapping mappedId (Qualified remoteId domain)

-- | Only a single namespace/table is used for for potentially multiple different types of
-- mapped IDs.
insertIdMapping :: IdMapping a -> AppIO ()
insertIdMapping idMapping = do
  retry x5 $ write idMappingInsert (params Quorum (mappedId, remoteId, domain))
  where
    mappedId = _imMappedId idMapping
    remoteId = _qLocalPart (_imQualifiedId idMapping)
    domain = _qDomain (_imQualifiedId idMapping)

-------------------------------------------------------------------------------
-- Queries

idMappingSelect :: PrepQuery R (Identity (Id (Mapped a))) (Id (Remote a), Domain)
idMappingSelect =
  "select remote_id, remote_domain from id_mapping where mapped_id = ?"

idMappingInsert :: PrepQuery W (Id (Mapped a), Id (Remote a), Domain) ()
idMappingInsert =
  "insert into id_mapping (mapped_id, remote_id, remote_domain) values (?, ?, ?)"
