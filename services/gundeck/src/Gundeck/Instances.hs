{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module Gundeck.Instances
  (
  )
where

import Amazonka.Data
import Cassandra.CQL
import Data.Attoparsec.Text qualified as Parser
import Data.Id
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as Uuid
import Gundeck.Aws.Arn (EndpointArn)
import Imports

instance Cql EndpointArn where
  ctype = Tagged TextColumn
  toCql = CqlText . toText
  fromCql (CqlText txt) = fromText txt
  fromCql _ = Left "EndpointArn: Text expected"

instance ToText (Id a) where
  toText = Text.decodeUtf8 . Uuid.toASCIIBytes . toUUID

instance FromText (Id a) where
  fromText =
    Parser.parseOnly $
      Parser.take 36 >>= \txt ->
        txt
          & Text.encodeUtf8
          & Uuid.fromASCIIBytes
          & maybe (fail "Invalid UUID") (pure . Id)
