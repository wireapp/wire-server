module Test.Domain
  ( tests,
  )
where

import Data.Domain (DomainText (DomainText), domainText, mkDomain)
import qualified Data.Text as Text
import Imports
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Domain"
    [ testGroup "serialization" testDomainSerialization
    ]

testDomainSerialization :: [TestTree]
testDomainSerialization =
  [ testProperty "Arbitrary DomainText generates valid domains" $
      \(DomainText x) ->
        isRight $ mkDomain x,
    testProperty "parsing a domain normalizes it" $
      \(DomainText x) ->
        (domainText <$> mkDomain x) === Right (Text.toCaseFold x)
  ]
