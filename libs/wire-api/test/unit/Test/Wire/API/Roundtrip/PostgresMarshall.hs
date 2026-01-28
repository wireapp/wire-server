-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Wire.API.Roundtrip.PostgresMarshall (tests) where

import Crypto.Error (CryptoFailable (..))
import Crypto.KDF.Argon2 qualified as Argon2
import Data.ByteString.Char8 qualified as BS8
import Data.Code qualified as Code
import Data.Misc (PlainTextPassword8, fromPlainTextPassword)
import Data.Text.Encoding (encodeUtf8)
import Imports
import Test.Tasty qualified as T
import Test.Tasty.QuickCheck
import Type.Reflection (typeRep)
import Wire.API.Password as Password
import Wire.API.Password.Argon2id (Argon2HashedPassword (..), encodeArgon2HashedPassword)
import Wire.API.Password.Scrypt (encodeScryptPassword)
import Wire.API.PostgresMarshall
import Wire.Arbitrary qualified as Arbitrary ()

tests :: T.TestTree
tests =
  T.localOption (T.Timeout (60 * 1000000) "60s") . T.testGroup "PostgresMarshall roundtrip tests" $
    [ testRoundTrip @Text @Code.Key,
      testRoundTrip @Text @Code.Value,
      testRoundTrip @ByteString @Password.Password
    ]

testRoundTrip ::
  forall db domain.
  (Arbitrary domain, Typeable domain, PostgresMarshall db domain, PostgresUnmarshall db domain, Eq domain, Show domain) =>
  T.TestTree
testRoundTrip = testProperty msg trip
  where
    msg = show (typeRep @domain)
    trip (value :: domain) =
      counterexample (show value) $
        Right value === (postgresUnmarshall . postgresMarshall @db) value

instance Arbitrary Password where
  arbitrary = Argon2Password . hashPlaintext <$> (arbitrary :: Gen PlainTextPassword8)
    where
      hashPlaintext plain =
        let opts =
              Argon2.Options
                { variant = Argon2.Argon2id,
                  version = Argon2.Version13,
                  iterations = 1,
                  parallelism = 1,
                  memory = 8
                }
            salt = BS8.pack "static-salt-1234"
            password = encodeUtf8 (fromPlainTextPassword plain)
            hashedKey = hashWithOptions opts password salt
         in Argon2HashedPassword {opts, salt, hashedKey}
      hashWithOptions opts password salt =
        let tagSize = 16
         in case Argon2.hash opts password salt tagSize of
              CryptoFailed err -> error $ "argon2 hash failed: " <> show err
              CryptoPassed hash -> hash

instance Eq Password where
  p1 == p2 = passwordText p1 == passwordText p2
    where
      passwordText = \case
        Argon2Password p -> encodeArgon2HashedPassword p
        ScryptPassword p -> encodeScryptPassword p
