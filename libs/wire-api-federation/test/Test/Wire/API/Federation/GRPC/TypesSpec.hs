-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Federation.GRPC.TypesSpec where

import Data.Domain (domainText, mkDomain)
import Data.Either.Validation
import Imports
import Mu.Schema (FromSchema (fromSchema), ToSchema (toSchema))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary (..), Gen, Property, counterexample, forAll, oneof, suchThat)
import Type.Reflection (typeRep)
import Wire.API.Federation.GRPC.Types

spec :: Spec
spec =
  describe "Wire.API.Federation.GRPC.Types" $ do
    describe "Protobuf Serialization" $ do
      muSchemaRoundtrip @Router @"Component" @Component
      muSchemaRoundtrip @Router @"HTTPResponse" @HTTPResponse
      muSchemaRoundtrip @Router @"OutwardResponse" @OutwardResponse
      muSchemaRoundtrip @Router @"InwardResponse" @InwardResponse
      muSchemaRoundtrip @Router @"Method" @HTTPMethod
      muSchemaRoundtrip @Router @"QueryParam" @QueryParam
      muSchemaRoundtrip @Router @"Request" @Request
      muSchemaRoundtrip @Router @"FederatedRequest" @FederatedRequest

    describe "validateFederatedRequest" $ do
      prop "should succeed when FederatedRequest is valid" $ do
        let callGen = FederatedRequest <$> validDomain <*> (Just <$> arbitrary)
        forAll callGen $ \c -> isRight' (validateFederatedRequest c)
      prop "should fail appropriately when domain is not valid" $ do
        let callGen = FederatedRequest <$> invalidDomain <*> arbitrary
        forAll callGen $ \c -> counterexample ("validation result: " <> show (validateFederatedRequest c)) $
          case validateFederatedRequest c of
            Success _ -> False
            Failure errs ->
              any
                ( \case
                    InvalidDomain _ -> True
                    _ -> False
                )
                errs
      prop "should fail appropriately when request is missing" $ do
        let -- Here using 'arbitrary' for generating domain will mostly generate invalid domains
            callGen = FederatedRequest <$> maybeValidDomainTextGen <*> pure Nothing
        forAll callGen $ \c -> counterexample ("validation result: " <> show (validateFederatedRequest c)) $
          case validateFederatedRequest c of
            Success _ -> False
            Failure errs ->
              any
                ( \case
                    RequestMissing -> True
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

muSchemaRoundtrip :: forall sch sty a. (ToSchema sch sty a, FromSchema sch sty a, Show a, Eq a, Arbitrary a, Typeable a) => Spec
muSchemaRoundtrip =
  prop ("Mu Schema Roundtrip: " <> show (typeRep @a)) $ \(x :: a) ->
    fromSchema (toSchema @_ @_ @sch @sty x) == x
