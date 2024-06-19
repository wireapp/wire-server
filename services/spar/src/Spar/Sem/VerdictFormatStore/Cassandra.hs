-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module Spar.Sem.VerdictFormatStore.Cassandra
  ( verdictFormatStoreToCassandra,
  )
where

import Cassandra as Cas
import Control.Lens
import Data.Time
import Imports
import Polysemy
import Spar.Data
import Spar.Data.Instances (VerdictFormatCon, VerdictFormatRow, fromVerdictFormat, toVerdictFormat)
import Spar.Sem.VerdictFormatStore
import URI.ByteString
import Wire.API.User.Saml

verdictFormatStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (VerdictFormatStore ': r) a ->
  Sem r a
verdictFormatStoreToCassandra = interpret $ \case
  Store ndt itla vf -> embed @m $ storeVerdictFormat ndt itla vf
  Get itla -> embed @m $ getVerdictFormat itla

-- | First argument is the life expectancy of the request.  (We store the verdict format for twice
-- as long.  Reason: if there is some delay in processing a very old request, it would be bad for
-- error handling if we couldn't figure out where the error will land.)
storeVerdictFormat ::
  (HasCallStack, MonadClient m) =>
  NominalDiffTime ->
  AReqId ->
  VerdictFormat ->
  m ()
storeVerdictFormat diffTime req (fromVerdictFormat -> (fmtCon, fmtMobSucc, fmtMobErr)) = do
  let ttl = nominalDiffToSeconds diffTime * 2
  retry x5 . write cql $ params LocalQuorum (req, fmtCon, fmtMobSucc, fmtMobErr, ttl)
  where
    cql :: PrepQuery W (AReqId, VerdictFormatCon, Maybe URI, Maybe URI, Int32) ()
    cql = "INSERT INTO verdict (req, format_con, format_mobile_success, format_mobile_error) VALUES (?, ?, ?, ?) USING TTL ?"

getVerdictFormat ::
  (HasCallStack, MonadClient m) =>
  AReqId ->
  m (Maybe VerdictFormat)
getVerdictFormat req =
  (>>= toVerdictFormat)
    <$> (retry x1 . query1 cql $ params LocalQuorum (Identity req))
  where
    cql :: PrepQuery R (Identity AReqId) VerdictFormatRow
    cql = "SELECT format_con, format_mobile_success, format_mobile_error FROM verdict WHERE req = ?"
