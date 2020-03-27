module Test.Domain
  ( tests,
  )
where

import Data.Domain (DomainText (DomainText), domainText, mkDomain)
import qualified Data.Text as Text
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Domain"
    [ testGroup "serialization" testDomainSerialization
    ]

testDomainSerialization :: [TestTree]
testDomainSerialization =
  [ testCase "parses some example domains" $ do
      let validDomains =
            [ "wire.com",
              "some.long.example.with.lots.of.labels.com",
              "multiple-------dashes.co.uk",
              "0123456789.abcdefghijklmnopqrstuvwxyz.ABCDEFGHIJKLMNOPQRSTUVWXYZ",
              "Domain-with-dashes-and-Uppercase.Even-in-TLD",
              "1.1.1.g1", -- just TLD can't start with digit
              Text.replicate 63 "x" <> ".foo",
              Text.intercalate "." (replicate 127 "h") -- 253 chars
            ]
      for_ validDomains $ \d ->
        case mkDomain d of
          Right _ -> pure ()
          Left err -> assertFailure $ "valid domain " <> show d <> " not parsed successfully: " <> err,
    testCase "rejects invalid domains" $ do
      let invalidDomains =
            [ "dotless-domain",
              "label.starting.with.-dash.com",
              "label.ending.with.dash-.com",
              "exämple.com",
              "(comment)domain.com",
              "[1.1.1.1]", -- bracketed IP
              "192.168.1.65", -- IP
              Text.replicate 64 "x" <> ".foo",
              Text.intercalate "." (replicate 128 "h") -- 255 chars
            ]
      for_ invalidDomains $ \h ->
        case mkDomain h of
          Left _ -> pure ()
          Right parsed -> assertFailure $ "invalid domain parsed successfully: " <> show (h, parsed),
    testProperty "Arbitrary DomainText generates valid domains" $
      \(DomainText x) ->
        isRight $ mkDomain x,
    testProperty "parsing a domain normalizes it" $
      \(DomainText x) ->
        (domainText <$> mkDomain x) === Right (Text.toCaseFold x)
  ]
