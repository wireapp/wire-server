{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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

module Test.Wire.API.Federation.ClientSpec where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Domain (Domain (Domain))
import qualified Data.Text as Text
import Imports
import qualified Mu.Server as Mu
import Test.Hspec
import Test.QuickCheck (arbitrary, generate)
import qualified Wire.API.Federation.API.Brig as Brig
import Wire.API.Federation.Client (FederationClientError (FederationClientOutwardError, FederationClientRPCError))
import Wire.API.Federation.GRPC.Types (Component (Brig), FederatedRequest (FederatedRequest), Request (..))
import Wire.API.Federation.Mock
import Wire.API.User (UserProfile)

spec :: Spec
spec = do
  stateRef <- runIO $ initState (Domain "target.example.com") (Domain "origin.example.com")
  beforeAll (assertRightT (startMockFederator stateRef))
    . afterAll_ (stopMockFederator stateRef)
    . before_ (flushState stateRef)
    $ describe "Federator.Client" $ do
      it "should make correct calls to the federator and parse success response correctly" $ do
        handle <- generate arbitrary
        expectedResponse :: Maybe UserProfile <- generate arbitrary

        (actualResponse, sentRequests) <-
          assertRightT . withMockFederatorClient stateRef (mkSuccessResponse expectedResponse) $
            Brig.getUserByHandle Brig.clientRoutes handle

        sentRequests `shouldBe` [FederatedRequest "target.example.com" (Just $ Request Brig "/federation/get-user-by-handle" (LBS.toStrict (Aeson.encode handle)) "origin.example.com")]
        actualResponse `shouldBe` Right expectedResponse

      it "should parse failure response correctly" $ do
        handle <- generate arbitrary
        someErr <- generate arbitrary

        (actualResponse, _) <-
          assertRightT . withMockFederatorClient stateRef (mkErrorResponse someErr) $
            Brig.getUserByHandle Brig.clientRoutes handle

        actualResponse `shouldBe` Left (FederationClientOutwardError someErr)

      it "should report federator failures correctly" $ do
        handle <- generate arbitrary

        (actualResponse, _) <-
          assertRightT . withMockFederatorClient stateRef (error "some IO error!") $
            Brig.getUserByHandle Brig.clientRoutes handle

        case actualResponse of
          Right res ->
            expectationFailure $ "Expected response to be failure, got: \n" <> show res
          Left (FederationClientRPCError errText) ->
            Text.unpack errText `shouldStartWith` "grpc error: GRPC status indicates failure: status-code=INTERNAL, status-message=\"some IO error!"
          Left err ->
            expectationFailure $ "Expected FedeartionClientRPCError, got different error: \n" <> show err

      it "should report GRPC errors correctly" $ do
        handle <- generate arbitrary

        (actualResponse, _) <-
          assertRightT . withMockFederatorClient stateRef (throwError $ Mu.ServerError Mu.NotFound "Just testing") $
            Brig.getUserByHandle Brig.clientRoutes handle

        actualResponse `shouldBe` Left (FederationClientRPCError "grpc error: GRPC status indicates failure: status-code=NOT_FOUND, status-message=\"Just testing\"")

assertRight :: Either String b -> IO b
assertRight = \case
  Left a -> do
    expectationFailure $ "Expected Right, got Left: " <> a
    error "impossible"
  Right b -> pure b

assertRightT :: ExceptT String IO b -> IO b
assertRightT m = runExceptT m >>= assertRight
