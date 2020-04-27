{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Brig.Types.Instances
  (
  )
where

import Brig.Types.Team.LegalHold
import Cassandra.CQL
import Data.ByteString.Conversion
import Imports
import Wire.API.Service.Provider
import Wire.API.Service.Provider.Tag
import Wire.API.User.Client.Prekey

instance Cql LegalHoldStatus where
  ctype = Tagged IntColumn

  fromCql (CqlInt n) = case n of
    0 -> pure $ LegalHoldDisabled
    1 -> pure $ LegalHoldEnabled
    _ -> fail "fromCql: Invalid LegalHoldStatus"
  fromCql _ = fail "fromCql: LegalHoldStatus: CqlInt expected"

  toCql LegalHoldDisabled = CqlInt 0
  toCql LegalHoldEnabled = CqlInt 1

instance Cql PrekeyId where
  ctype = Tagged IntColumn
  toCql = CqlInt . fromIntegral . keyId
  fromCql (CqlInt i) = return $ PrekeyId (fromIntegral i)
  fromCql _ = fail "PrekeyId: Int expected"

instance Cql ServiceTag where
  ctype = Tagged BigIntColumn

  fromCql (CqlBigInt i) = case intToTag i of
    Just t -> return t
    Nothing -> fail $ "unexpected service tag: " ++ show i
  fromCql _ = fail "service tag: int expected"

  toCql = CqlBigInt . tagToInt

instance Cql ServiceKeyPEM where
  ctype = Tagged BlobColumn

  fromCql (CqlBlob b) =
    maybe
      (fail "service key pem: malformed key")
      pure
      (fromByteString' b)
  fromCql _ = fail "service key pem: blob expected"

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
      0 -> return $! ServiceKey RsaServiceKey s p
      _ -> fail $ "Unexpected service key type: " ++ show t
    where
      required :: Cql r => Text -> Either String r
      required f =
        maybe
          (fail ("ServiceKey: Missing required field '" ++ show f ++ "'"))
          fromCql
          (lookup f fs)
  fromCql _ = fail "service key: udt expected"

  toCql (ServiceKey RsaServiceKey siz pem) =
    CqlUdt
      [ ("typ", CqlInt 0),
        ("size", toCql siz),
        ("pem", toCql pem)
      ]
