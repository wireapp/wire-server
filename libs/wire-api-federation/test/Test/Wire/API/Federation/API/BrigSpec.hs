{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Test.Wire.API.Federation.API.BrigSpec where

import Test.Hspec (Spec, describe, shouldBe, specify)
import Wire.API.Federation.API.Brig (SearchRequest(..))
import Test.QuickCheck (Arbitrary, counterexample, (===))
import Imports
import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON), Value)
import Data.Typeable (typeRep)
import Data.Proxy (Proxy (..))
import Data.Aeson.Types (parseEither)
import Test.Hspec.QuickCheck (prop)
import Data.Aeson.QQ.Simple (aesonQQ)

spec :: Spec
spec = describe "Wire.API.Federation.API.Brig" $ do
  describe "RoundTripTests" $ do
    jsonRoundTrip @SearchRequest
  describe "JSON Golden Tests" $ do
    jsonGoldenTest "SearchRequest" [aesonQQ|{"term": "searchedThing"}|] (SearchRequest "searchedThing")

-- | FUTUREWORK: Extract this into a library so it is not repeated everywhere.
jsonRoundTrip :: forall a. (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a) => Spec
jsonRoundTrip = prop msg trip
  where
    msg = show (typeRep (Proxy @a))
    trip (v :: a) =
      counterexample (show $ toJSON v) $
        Right v === (parseEither parseJSON . toJSON) v

jsonGoldenTest :: (Eq a, Show a, FromJSON a )=> String -> Value -> a -> Spec
jsonGoldenTest name val expected =
  specify ("GoldenTest: " <> show name) $ do
    parseEither parseJSON val `shouldBe` Right expected
