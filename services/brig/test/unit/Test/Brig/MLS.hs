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
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import Data.Time.Clock
import Imports
import Test.Tasty
import Test.Tasty.QuickCheck
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Extension
import Wire.API.MLS.Serialisation

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

newtype ValidExtensions = ValidExtensions [Extension]

instance Show ValidExtensions where
  show (ValidExtensions exts) = "ValidExtensions (length " <> show (length exts) <> ")"

unknownExt :: Gen Extension
unknownExt = do
  Positive t0 <- arbitrary
  let t = t0 + fromEnum (maxBound :: ExtensionTag) + 1
  Extension (fromIntegral t) <$> arbitrary

-- | Generate a list of extensions containing all the required ones.
instance Arbitrary ValidExtensions where
  arbitrary = do
    exts0 <- listOf unknownExt
    LifetimeAndExtension ext1 _ <- arbitrary
    exts2 <- listOf unknownExt
    CapabilitiesAndExtension ext3 _ <- arbitrary
    exts4 <- listOf unknownExt
    pure . ValidExtensions $ exts0 <> [ext1] <> exts2 <> [ext3] <> exts4

newtype InvalidExtensions = InvalidExtensions [Extension]

-- | Generate a list of extensions which does not contain one of the required extensions.
instance Show InvalidExtensions where
  show (InvalidExtensions exts) = "InvalidExtensions (length " <> show (length exts) <> ")"

instance Arbitrary InvalidExtensions where
  arbitrary = do
    req <- fromMLSEnum <$> elements [LifetimeExtensionTag, CapabilitiesExtensionTag]
    InvalidExtensions <$> listOf (arbitrary `suchThat` ((/= req) . extType))

data LifetimeAndExtension = LifetimeAndExtension Extension Lifetime
  deriving (Show)

instance Arbitrary LifetimeAndExtension where
  arbitrary = do
    lt <- arbitrary
    let ext = Extension (fromIntegral (fromEnum LifetimeExtensionTag + 1)) . LBS.toStrict . runPut $ do
          put (timestampSeconds (ltNotBefore lt))
          put (timestampSeconds (ltNotAfter lt))
    pure $ LifetimeAndExtension ext lt

data CapabilitiesAndExtension = CapabilitiesAndExtension Extension Capabilities
  deriving (Show)

instance Arbitrary CapabilitiesAndExtension where
  arbitrary = do
    caps <- arbitrary
    let ext = Extension (fromIntegral (fromEnum CapabilitiesExtensionTag + 1)) . LBS.toStrict . runPut $ do
          putWord8 (fromIntegral (length (capVersions caps)))
          traverse_ (putWord8 . pvNumber) (capVersions caps)

          putWord8 (fromIntegral (length (capCiphersuites caps) * 2))
          traverse_ (put . cipherSuiteNumber) (capCiphersuites caps)

          putWord8 (fromIntegral (length (capExtensions caps) * 2))
          traverse_ put (capExtensions caps)

          putWord8 (fromIntegral (length (capProposals caps) * 2))
          traverse_ put (capProposals caps)
    pure $ CapabilitiesAndExtension ext caps

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
      testGroup
        "Extensions"
        [ testProperty "required extensions are found" $ \(ValidExtensions exts) ->
            isRight (findExtensions exts),
          testProperty "missing required extensions" $ \(InvalidExtensions exts) ->
            isLeft (findExtensions exts),
          testProperty "lifetime extension" $ \(LifetimeAndExtension ext lt) ->
            decodeExtension ext == Right (Just (SomeExtension SLifetimeExtensionTag lt)),
          testProperty "capabilities extension" $ \(CapabilitiesAndExtension ext caps) ->
            decodeExtension ext == Right (Just (SomeExtension SCapabilitiesExtensionTag caps))
        ]
    ]
