module Test.Qualified
  ( tests,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Conversion as BS.C
import Data.Domain (Domain (Domain), DomainText (DomainText), domainText, mkDomain)
import Data.Handle (Handle (Handle, fromHandle), parseHandleEither)
import Data.Id (Id (Id, toUUID), UserId)
import Data.Qualified (OptionallyQualified, Qualified (Qualified), eitherQualifiedOrNot, mkQualifiedHandle, mkQualifiedId, renderQualifiedHandle, renderQualifiedId)
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.E
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
    [ testGroup "Handle" testHandle,
      testGroup "Domain" testDomain,
      testGroup "Qualified" testQualified
    ]

testHandle :: [TestTree]
testHandle =
  [ testCase "parses some example handles" $ do
      let validHandles = ["handle", "--", "__", "..", "0123456789", Text.replicate 256 "a"]
      for_ validHandles $ \h ->
        case parseHandleEither h of
          Right _ -> pure ()
          Left err -> assertFailure $ "valid handle " <> show h <> " not parsed successfully: " <> err,
    testCase "rejects invalid handles" $ do
      let invalidHandles =
            [ "h", -- too short
              Text.replicate 257 "a", -- too long
              "myhändle",
              "myhàndle",
              "myh@ndle",
              "$pecial",
              "some+name",
              "upperCase",
              "with space"
            ]
      for_ invalidHandles $ \h ->
        case parseHandleEither h of
          Left _ -> pure ()
          Right parsed -> assertFailure $ "invalid handle parsed successfully: " <> show (h, parsed),
    testProperty "roundtrip for Handle" $
      \(x :: Handle) ->
        parseHandleEither (fromHandle x) === Right x
  ]

testDomain :: [TestTree]
testDomain =
  [ testProperty "Arbitrary DomainText generates valid domains" $
      \(DomainText x) ->
        isRight $ mkDomain x,
    testProperty "parsing a domain normalizes it" $
      \(DomainText x) ->
        (domainText <$> mkDomain x) === Right (Text.toCaseFold x)
  ]

testQualified :: [TestTree]
testQualified =
  [ testGroup "serialization" testQualifiedSerialization
  ]

testQualifiedSerialization :: [TestTree]
testQualifiedSerialization =
  [ testCase "render foo@bar.com" $ do
      assertEqual "" "foo@bar.com" $
        (renderQualifiedHandle (Qualified (Handle "foo") (Domain "bar.com"))),
    testCase "render 61a73a52-e526-4892-82a9-3d638d77629f@example.com" $ do
      uuid <-
        maybe (assertFailure "invalid UUID") pure $
          UUID.fromString "61a73a52-e526-4892-82a9-3d638d77629f"
      assertEqual "" "61a73a52-e526-4892-82a9-3d638d77629f@example.com" $
        (renderQualifiedId (Qualified (Id uuid) (Domain "example.com"))),
    testProperty "roundtrip for Qualified Handle" $
      \(x :: Qualified Handle) ->
        mkQualifiedHandle (renderQualifiedHandle x) === Right x,
    testProperty "roundtrip for Qualified UserId" $
      \(x :: Qualified UserId) ->
        mkQualifiedId (renderQualifiedId x) === Right x,
    testProperty "roundtrip for OptionallyQualified Handle" $
      \(x :: OptionallyQualified Handle) -> do
        let render = Text.E.encodeUtf8 . either fromHandle renderQualifiedHandle . eitherQualifiedOrNot
        let parse = BS.C.runParser BS.C.parser
        parse (render x) === Right x,
    testProperty "roundtrip for OptionallyQualified UserId" $
      \(x :: OptionallyQualified UserId) -> do
        let render = Text.E.encodeUtf8 . either (cs . UUID.toString . toUUID) renderQualifiedId . eitherQualifiedOrNot
        let parse = BS.C.runParser BS.C.parser
        parse (render x) === Right x,
    jsonRoundtrip @(Qualified Handle),
    jsonRoundtrip @(Qualified UserId)
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
