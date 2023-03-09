module Test.Wire.API.Routes.Version where

import Data.Aeson as Aeson
import Data.Binary.Builder
import Data.ByteString.Conversion
import Data.String.Conversions (cs)
import Imports
import Servant.API
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Routes.Version

{-# ANN tests ("HLint: ignore Functor law" :: String) #-}
tests :: TestTree
tests =
  testGroup
    "Version always has the shape V<int>; serializations of Version and VersionNumber are `v<int>`, `<int>`, resp.  <int> is non-negative."
    [ testCase "Version, show, 'v' prefix" $ do
        nub (toLower . head . show <$> allVersions) @=? ['v'],
      testCase "Version, show, int suffix" $ do
        let expected = show $ (read @Int) . tail . show <$> allVersions
        assertBool expected (isJust (Aeson.decode @[Int] (cs expected))),
      testGroup "Version: all serializations are the same as `show`, up to string type" $ do
        [ testCase "toByteString'" $ fmap toLower . show <$> allVersions @=? cs . toByteString' <$> allVersions,
          testCase "encode" $ fmap toLower (show (show <$> allVersions)) @=? cs (encode allVersions), -- (`encode @Version` has extra double-quotes)
          testCase "toUrlPiece" $ fmap toLower . show <$> allVersions @=? cs . toUrlPiece <$> allVersions,
          testCase "toEncodedUrlPiece" $ fmap toLower . show <$> allVersions @=? cs . toLazyByteString . toEncodedUrlPiece <$> allVersions,
          testCase "toHeader" $ fmap toLower . show <$> allVersions @=? cs . toHeader <$> allVersions,
          testCase "toQueryParam" $ fmap toLower . show <$> allVersions @=? cs . toQueryParam <$> allVersions
          ],
      testGroup "VersionNumber: all serializations are the same as `tail . show . fromVersionNumber`, up to string type" $
        [ testCase "toByteString'" $ tail . show . fromVersionNumber <$> allVersionNumbers @=? cs . toByteString' <$> allVersionNumbers,
          testCase "encode" $ tail . show . fromVersionNumber <$> allVersionNumbers @=? cs . encode <$> allVersionNumbers,
          testCase "toUrlPiece" $ tail . show . fromVersionNumber <$> allVersionNumbers @=? cs . toUrlPiece <$> allVersionNumbers,
          testCase "toEncodedUrlPiece" $ tail . show . fromVersionNumber <$> allVersionNumbers @=? cs . toLazyByteString . toEncodedUrlPiece <$> allVersionNumbers,
          testCase "toHeader" $ tail . show . fromVersionNumber <$> allVersionNumbers @=? cs . toHeader <$> allVersionNumbers,
          testCase "toQueryParam" $ tail . show . fromVersionNumber <$> allVersionNumbers @=? cs . toQueryParam <$> allVersionNumbers
        ]
    ]

allVersions :: [Version]
allVersions = [minBound ..]

allVersionNumbers :: [VersionNumber]
allVersionNumbers = [minBound ..]
