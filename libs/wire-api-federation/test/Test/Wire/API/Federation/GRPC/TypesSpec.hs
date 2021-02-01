module Test.Wire.API.Federation.GRPC.TypesSpec where

import Data.Domain (domainText, mkDomain)
import Data.Either.Validation
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary (..), Gen, Property, counterexample, forAll, oneof, suchThat)
import Wire.API.Federation.GRPC.Types

spec :: Spec
spec =
  describe "Wire.API.Federation.GRPC.Types" $ do
    describe "validateRemoteCall" $ do
      prop "should succeed when RemoteCall is valid" $ do
        let callGen = RemoteCall <$> validDomain <*> (Just <$> arbitrary)
        forAll callGen $ \c -> isRight' (validateRemoteCall c)
      prop "should fail appropriately when domain is not valid" $ do
        let callGen = RemoteCall <$> invalidDomain <*> arbitrary
        forAll callGen $ \c -> counterexample ("validation result: " <> show (validateRemoteCall c)) $
          case validateRemoteCall c of
            Success _ -> False
            Failure errs ->
              any
                ( \case
                    InvalidDomain _ -> True
                    _ -> False
                )
                errs
      prop "should fail appropriately when localCall is missing" $ do
        let -- Here using 'arbitrary' for generating domain will mostly generate invalid domains
            callGen = RemoteCall <$> maybeValidDomainTextGen <*> pure Nothing
        forAll callGen $ \c -> counterexample ("validation result: " <> show (validateRemoteCall c)) $
          case validateRemoteCall c of
            Success _ -> False
            Failure errs ->
              any
                ( \case
                    LocalCallMissing -> True
                    _ -> False
                )
                errs

isRight' :: (Show a, Show b) => Validation a b -> Property
isRight' x = counterexample ("validation result: " <> show x) $ isRight $ validationToEither x

-- | Generates uniform distribution of valid and invalid domains
maybeValidDomainTextGen :: Gen Text
maybeValidDomainTextGen = oneof [invalidDomain, validDomain]

validDomain :: Gen Text
validDomain = domainText <$> arbitrary

invalidDomain :: Gen Text
invalidDomain = arbitrary `suchThat` (isLeft . mkDomain)
