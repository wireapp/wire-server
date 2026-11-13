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

module Test.Brig.MLS where

import Brig.API.MLS.KeyPackages.Validation
import Data.Binary
import Data.Either
import Data.Map.Strict qualified as Map
import Data.Time.Clock
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Lifetime

-- | A lifetime with a length of at least 1 day.
newtype ValidLifetime = ValidLifetime Lifetime
  deriving (Show)

instance Arbitrary ValidLifetime where
  arbitrary =
    ValidLifetime <$> do
      -- all values are 32 bits to avoid overflow
      t1 <- promote <$> arbitrary
      dt <- promote <$> arbitrary
      pure $ Lifetime (Timestamp t1) (Timestamp (t1 + dt + 86400))
    where
      promote :: Word32 -> Word64
      promote = fromIntegral

midpoint :: Lifetime -> NominalDiffTime
midpoint lt =
  secondsToNominalDiffTime
    ( fromInteger
        ( div
            ( fromIntegral (timestampSeconds (ltNotBefore lt))
                + fromIntegral (timestampSeconds (ltNotBefore lt))
            )
            2
        )
    )

tests :: TestTree
tests =
  testGroup
    "MLS"
    [ testGroup
        "Lifetime"
        [ testProperty "not_before in the future" $ \lt ->
            isLeft $
              validateLifetime'
                (secondsToNominalDiffTime (fromIntegral (timestampSeconds (ltNotBefore lt) - 86400)))
                Nothing
                lt,
          testProperty "not_after in the past" $ \lt ->
            isLeft $
              validateLifetime'
                (secondsToNominalDiffTime (fromIntegral (timestampSeconds (ltNotAfter lt) + 86400)))
                Nothing
                lt,
          testProperty "valid" $ \(ValidLifetime lt) ->
            isRight $ validateLifetime' (midpoint lt) Nothing lt,
          testProperty "expiration too far" $ \(ValidLifetime lt) ->
            isLeft $ validateLifetime' (midpoint lt) (Just 10) lt
        ],
      testCase "key package lookup is cached per signature scheme" $ do
        calls <- newIORef []
        let lookupKey scheme = do
              modifyIORef' calls (<> [scheme])
              pure (Just (show scheme))
        (_, cache1) <- lookupWithCache lookupKey Ed25519 Map.empty
        (_, cache2) <- lookupWithCache lookupKey Ed25519 cache1
        (_, cache3) <- lookupWithCache lookupKey Ecdsa_secp256r1_sha256 cache2
        (_, _) <- lookupWithCache lookupKey Ecdsa_secp256r1_sha256 cache3
        readIORef calls >>= \actual ->
          assertEqual
            "each signature scheme is looked up once"
            [Ed25519, Ecdsa_secp256r1_sha256]
            actual
    ]
