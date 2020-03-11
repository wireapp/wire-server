module Test.Qualified
  ( tests,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson.Types as Aeson
import Data.Domain (Domain (Domain))
import Data.Handle (Handle (Handle))
import Data.Id (Id (Id), UserId)
import Data.Qualified (Qualified (Qualified), mkQualifiedHandle, mkQualifiedId, renderQualifiedHandle, renderQualifiedId)
import qualified Data.UUID as UUID
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Type.Reflection (typeRep)

tests :: TestTree
tests =
  testGroup
    "Qualified"
    [ testGroup
        "serialization"
        [ testCase "render foo@bar.com" $ do
            assertEqual "" "foo@bar.com" $
              (renderQualifiedHandle (Qualified (Handle "foo") (Domain "bar.com"))),
          testCase "render 61a73a52-e526-4892-82a9-3d638d77629f@example.com" $ do
            uuid <-
              maybe (assertFailure "invalid UUID") pure $
                UUID.fromString "61a73a52-e526-4892-82a9-3d638d77629f"
            assertEqual "" "61a73a52-e526-4892-82a9-3d638d77629f@example.com" $
              (renderQualifiedId (Qualified (Id uuid) (Domain "example.com"))),
          testProperty "mkQualifiedHandle (renderQualifiedHandle x) == Right x" $
            \(x :: Qualified Handle) ->
              mkQualifiedHandle (renderQualifiedHandle x) === Right x,
          testProperty "mkQualifiedId (renderQualifiedId x) == Right x" $
            \(x :: Qualified UserId) ->
              mkQualifiedId (renderQualifiedId x) === Right x,
          jsonRoundtrip @(Qualified Handle),
          jsonRoundtrip @(Qualified UserId)
        ]
    ]

jsonRoundtrip ::
  forall a.
  (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a) =>
  TestTree
jsonRoundtrip = testProperty msg trip
  where
    msg = "jsonRoundTrip @(" <> show (typeRep @a) <> ")"
    trip (v :: a) =
      counterexample (show $ toJSON v) $
        Right v === (Aeson.parseEither parseJSON . toJSON) v
