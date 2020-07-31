{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module Gundeck.Instances
  (
  )
where

import Cassandra.CQL
import qualified Data.Attoparsec.Text as Parser
import qualified Data.ByteString.Lazy as Bytes
import Data.Id
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as Uuid
import Gundeck.Aws.Arn (EndpointArn)
import Gundeck.Types
import Imports
import Network.AWS.Data

instance Cql Transport where
  ctype = Tagged IntColumn

  toCql GCM = CqlInt 0
  toCql APNS = CqlInt 1
  toCql APNSSandbox = CqlInt 2
  toCql APNSVoIP = CqlInt 3
  toCql APNSVoIPSandbox = CqlInt 4

  fromCql (CqlInt i) = case i of
    0 -> return GCM
    1 -> return APNS
    2 -> return APNSSandbox
    3 -> return APNSVoIP
    4 -> return APNSVoIPSandbox
    n -> fail $ "unexpected transport: " ++ show n
  fromCql _ = fail "transport: int expected"

instance Cql ConnId where
  ctype = Tagged BlobColumn

  toCql (ConnId c) = CqlBlob (Bytes.fromStrict c)

  fromCql (CqlBlob b) = return . ConnId $ Bytes.toStrict b
  fromCql _ = fail "ConnId: Blob expected"

instance Cql EndpointArn where
  ctype = Tagged TextColumn
  toCql = CqlText . toText
  fromCql (CqlText txt) = either fail return (fromText txt)
  fromCql _ = fail "EndpointArn: Text expected"

instance Cql Token where
  ctype = Tagged TextColumn
  toCql = CqlText . tokenText
  fromCql (CqlText txt) = Right (Token txt)
  fromCql _ = fail "Token: Text expected"

instance Cql AppName where
  ctype = Tagged TextColumn
  toCql = CqlText . appNameText
  fromCql (CqlText txt) = Right (AppName txt)
  fromCql _ = fail "App: Text expected"

instance ToText (Id a) where
  toText = Text.decodeUtf8 . Uuid.toASCIIBytes . toUUID

instance FromText (Id a) where
  parser =
    Parser.take 36 >>= \txt ->
      txt & Text.encodeUtf8
        & Uuid.fromASCIIBytes
        & maybe (fail "Invalid UUID") (return . Id)
