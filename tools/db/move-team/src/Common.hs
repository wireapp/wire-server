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

module Common where

import Cassandra
import Conduit
import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Conduit.Combinators qualified as C
import Imports
import System.IO

sourceJsonLines :: (MonadIO m, FromJSON c) => Handle -> ConduitM a c m ()
sourceJsonLines handle =
  C.sourceHandle handle
    .| C.linesUnboundedAscii
    .| mapC (either error id . eitherDecodeStrict)

sinkJsonLines :: (ToJSON a) => Handle -> ConduitT [a] Void IO ()
sinkJsonLines hd = C.mapM_ (mapM_ (LBS.hPutStr hd . (<> "\n") . encode))

-- FUTUREWORK: this is very slow. Look for alterantives. Maybe `batch` queries are faster.
sinkTableRows :: (Tuple a) => PrepQuery W a () -> ConduitM a Void Client ()
sinkTableRows insertQuery = go
  where
    go = do
      mbTuple <- await
      case mbTuple of
        Nothing -> pure ()
        Just tuple -> do
          lift $ write insertQuery (params LocalQuorum tuple)
          go
