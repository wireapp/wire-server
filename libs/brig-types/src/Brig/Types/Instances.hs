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

module Brig.Types.Instances () where

import Brig.Types.Provider.Tag
import Cassandra.CQL
import Data.ByteString.Conversion
import Imports
import Wire.API.Provider
import Wire.API.Provider.Service
import Wire.API.User.Client.Prekey

instance Cql PrekeyId where
  ctype = Tagged IntColumn
  toCql = CqlInt . fromIntegral . keyId
  fromCql (CqlInt i) = pure $ PrekeyId (fromIntegral i)
  fromCql _ = Left "PrekeyId: Int expected"

instance Cql ServiceTag where
  ctype = Tagged BigIntColumn

  fromCql (CqlBigInt i) = case intToTag i of
    Just t -> pure t
    Nothing -> Left $ "unexpected service tag: " ++ show i
  fromCql _ = Left "service tag: int expected"

  toCql = CqlBigInt . tagToInt

instance Cql ServiceKeyPEM where
  ctype = Tagged BlobColumn

  fromCql (CqlBlob b) =
    maybe
      (Left "service key pem: malformed key")
      pure
      (fromByteString' b)
  fromCql _ = Left "service key pem: blob expected"

  toCql = CqlBlob . toByteString

instance Cql ServiceKey where
  ctype =
    Tagged
      ( UdtColumn
          "pubkey"
          [ ("typ", IntColumn),
            ("size", IntColumn),
            ("pem", BlobColumn)
          ]
      )

  fromCql (CqlUdt fs) = do
    t <- required "typ"
    s <- required "size"
    p <- required "pem"
    case (t :: Int32) of
      0 -> pure $! ServiceKey RsaServiceKey s p
      _ -> Left $ "Unexpected service key type: " ++ show t
    where
      required :: (Cql r) => Text -> Either String r
      required f =
        maybe
          (Left ("ServiceKey: Missing required field '" ++ show f ++ "'"))
          fromCql
          (lookup f fs)
  fromCql _ = Left "service key: udt expected"

  toCql (ServiceKey RsaServiceKey siz pem) =
    CqlUdt
      [ ("typ", CqlInt 0),
        ("size", toCql siz),
        ("pem", toCql pem)
      ]
