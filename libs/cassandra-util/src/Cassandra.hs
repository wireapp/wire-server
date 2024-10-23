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

-- | The top import of our Cassandra utility library. Essentially, a "everyone who needs cassandra needs this" import.
module Cassandra
  ( module C,
  )
where

-- pull in our extended wrapper of Database.CQL.Protocol
import Cassandra.CQL as C
  ( Ascii (Ascii),
    BatchType (BatchLogged, BatchUnLogged),
    Blob (Blob),
    ColumnType (AsciiColumn, BigIntColumn, BlobColumn, BooleanColumn, DoubleColumn, IntColumn, ListColumn, MaybeColumn, TextColumn, TimestampColumn, UdtColumn, UuidColumn, VarCharColumn),
    Consistency (All, LocalQuorum, One), -- DO NOT EXPORT 'Quorum' here (until a DC migration is complete)
    Cql,
    Keyspace (Keyspace),
    PagingState (..),
    QueryParams (QueryParams),
    QueryString (QueryString),
    R,
    S,
    Set (Set),
    Tagged (Tagged),
    TimeUuid (TimeUuid),
    Tuple (),
    Value (CqlAscii, CqlBigInt, CqlBlob, CqlBoolean, CqlDouble, CqlInt, CqlList, CqlText, CqlUdt),
    Version (V4),
    W,
    ctype,
    fromAscii,
    fromBlob,
    fromCql,
    fromSet,
    fromTimeUuid,
    retag,
    toCql,
    unKeyspace,
    untag,
  )
-- pull in our extended wrapper of Database.CQL.IO.
import Cassandra.Exec as C
  ( BatchM,
    Client,
    ClientState,
    MonadClient,
    Page (..),
    PageWithState (..),
    PrepQuery,
    Row,
    addPrepQuery,
    batch,
    emptyPage,
    hasMore,
    init,
    liftClient,
    localState,
    nextPage,
    paginate,
    paginateC,
    paginateWithState,
    params,
    paramsP,
    paramsPagingState,
    pwsHasMore,
    query,
    query1,
    result,
    retry,
    runClient,
    setConsistency,
    setType,
    shutdown,
    trans,
    write,
    x1,
    x5,
  )
import Cassandra.QQ as C (sql)
