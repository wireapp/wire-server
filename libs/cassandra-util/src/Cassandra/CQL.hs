-- | This module just exports components from cassandra's Database.CQL.Protocol.

module Cassandra.CQL (module C) where

import Database.CQL.Protocol as C (
  Keyspace(Keyspace),
  Tagged(Tagged),
  TimeUuid(TimeUuid),
  Blob(Blob),
  Ascii(Ascii),
  Set(Set),
  QueryString(QueryString),
  QueryParams(QueryParams),
  Consistency(One, Quorum, All),
  BatchType(BatchLogged, BatchUnLogged),
  Value(CqlInt, CqlBlob, CqlText, CqlUdt, CqlBigInt, CqlList, CqlAscii, CqlDouble, CqlBoolean),
  ColumnType(IntColumn, BlobColumn, TextColumn, BigIntColumn, UdtColumn, TimestampColumn, ListColumn, AsciiColumn, DoubleColumn, MaybeColumn, UuidColumn, BooleanColumn),
  Version(V4),
  R,
  W,
  S,
  Cql,
  unKeyspace,
  ctype,
  toCql,
  fromCql,
  fromAscii,
  fromSet,
  fromBlob,
  fromTimeUuid,
  retag,
  untag)
