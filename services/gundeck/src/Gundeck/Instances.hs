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
import Data.ByteString.Lazy qualified as Bytes
import Data.Id
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as Uuid
import Gundeck.Aws.Arn (EndpointArn)
import Gundeck.Types
import Imports

instance Cql Transport where
  ctype = Tagged IntColumn

  toCql GCM = CqlInt 0
  toCql APNS = CqlInt 1
  toCql APNSSandbox = CqlInt 2
  toCql APNSVoIP = CqlInt 3
  toCql APNSVoIPSandbox = CqlInt 4

  fromCql (CqlInt i) = case i of
    0 -> pure GCM
    1 -> pure APNS
    2 -> pure APNSSandbox
    3 -> pure APNSVoIP
    4 -> pure APNSVoIPSandbox
    n -> Left $ "unexpected transport: " ++ show n
  fromCql _ = Left "transport: int expected"

instance Cql ConnId where
  ctype = Tagged BlobColumn

  toCql (ConnId c) = CqlBlob (Bytes.fromStrict c)

  fromCql (CqlBlob b) = pure . ConnId $ Bytes.toStrict b
  fromCql _ = Left "ConnId: Blob expected"

instance Cql EndpointArn where
  ctype = Tagged TextColumn
  toCql = CqlText . toText
  fromCql (CqlText txt) = fromText txt
  fromCql _ = Left "EndpointArn: Text expected"

instance Cql Token where
  ctype = Tagged TextColumn
  toCql = CqlText . tokenText
  fromCql (CqlText txt) = Right (Token txt)
  fromCql _ = Left "Token: Text expected"

instance Cql AppName where
  ctype = Tagged TextColumn
  toCql = CqlText . appNameText
  fromCql (CqlText txt) = Right (AppName txt)
  fromCql _ = Left "App: Text expected"

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
