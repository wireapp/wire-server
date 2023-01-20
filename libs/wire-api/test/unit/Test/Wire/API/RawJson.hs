{-# LANGUAGE LambdaCase #-}

module Test.Wire.API.RawJson (tests) where

import Data.Aeson as A
import Data.Proxy
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import Imports
import Servant.API
import Test.Tasty
import Test.Tasty.QuickCheck
import Wire.API.RawJson

tests :: TestTree
tests =
  testGroup "RawJson" $
    [ testProperty "MimeUnrender and FromJSON produce same result" testMimeUnrenderAndFromJSON,
      testProperty "ToJSON FromJSON roundtrip" testJsonRoundTrip
    ]

testMimeUnrenderAndFromJSON :: Property
testMimeUnrenderAndFromJSON =
  forAll
    (arbitrary :: (Gen TL.Text))
    ( \t ->
        let muRes = mimeUnrender (Proxy :: Proxy JSON) (encodeUtf8 t)
            jsonRes = (fromJSON @RawJson . toJSON . RawJson . encodeUtf8) t
         in muRes == toEither jsonRes
    )
  where
    toEither :: Result a -> Either String a
    toEither = \case
      A.Success a -> Right a
      A.Error b -> Left b

testJsonRoundTrip :: Property
testJsonRoundTrip =
  forAll
    (arbitrary :: (Gen TL.Text))
    ( \t ->
        let val = (RawJson . encodeUtf8) t
         in (fromJSON . toJSON) val == A.Success val
    )
