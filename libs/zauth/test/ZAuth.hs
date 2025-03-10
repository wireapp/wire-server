{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module ZAuth
  ( tests,
  )
where

import Arbitraries ()
import Data.ByteString.Conversion
import Data.UUID.V4
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.ZAuth.Creation as C
import Data.ZAuth.Token
import Data.ZAuth.Validation as V
import Imports
import Polysemy
import Polysemy.Error
import Sodium.Crypto.Sign
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

runCreateAndValidate :: C.Env -> Vector PublicKey -> Sem '[ZAuthCreation, ZAuthValidation, Error Failure, Embed IO] a -> IO (Either Failure a)
runCreateAndValidate env pubKeys =
  runM
    . runError
    . V.interpretZAuthValidation pubKeys
    . C.interpretZAuthCreation env

tests :: IO TestTree
tests = do
  (p1, s1) <- newKeyPair
  (p2, s2) <- newKeyPair
  (p3, s3) <- newKeyPair
  z1 <- C.mkEnv 1 s1 [s2, s3]
  z2 <- C.mkEnv 2 s1 [s2, s3]
  z3 <- C.mkEnv 3 s1 [s2, s3]
  let pubKeys = Vector.fromList [p1, p2, p3]
  pure $
    testGroup
      "ZAuth"
      [ testGroup
          "Parsing"
          [ testProperty "decode . encode == id [access]" testDecEncAccessToken,
            testProperty "decode . encode == id [user]" testDecEncUserToken,
            testProperty "decode . encode == id [legalhold access]" testDecEncLegalHoldAccessToken,
            testProperty "decode . encode == id [legalhold user]" testDecEncLegalHoldUserToken,
            testProperty "decode as User . encode as LegalHoldUser == Nothing" testUserIsNotLegalHoldUser,
            testProperty "decode as LegalHoldUser . encode as User == Nothing" testUserIsNotLegalHoldUser'
          ],
        testGroup
          "Signing and Verifying"
          [ testCase "expired" (testExpired z1 pubKeys),
            testCase "not expired" (testNotExpired z2 pubKeys),
            testCase "signed access-token is valid" (testSignAndVerify z3 pubKeys)
          ]
      ]

defDuration :: Integer
defDuration = 1

testUserIsNotLegalHoldUser :: Token LU -> Property
testUserIsNotLegalHoldUser t = fromByteString @(Token U) (toByteString' t) === Nothing

testUserIsNotLegalHoldUser' :: Token U -> Property
testUserIsNotLegalHoldUser' t = fromByteString @(Token LU) (toByteString' t) === Nothing

testDecEncAccessToken :: Token A -> Property
testDecEncAccessToken t = fromByteString (toByteString' t) === Just t

testDecEncUserToken :: Token U -> Property
testDecEncUserToken t = fromByteString (toByteString' t) === Just t

testDecEncLegalHoldUserToken :: Token LU -> Property
testDecEncLegalHoldUserToken t = fromByteString (toByteString' t) === Just t

testDecEncLegalHoldAccessToken :: Token LA -> Property
testDecEncLegalHoldAccessToken t = fromByteString (toByteString' t) === Just t

testNotExpired :: C.Env -> Vector PublicKey -> IO ()
testNotExpired env pubKeys = do
  x <- runCreateAndValidate env pubKeys $ do
    u <- liftIO nextRandom
    t <- newToken @_ @U (TokenExpiresAfter defDuration) Nothing $ User u Nothing 100
    check t
  Right () @=? x

-- The testExpired test conforms to the following testing standards:
-- @SF.Channel @TSFI.RESTfulAPI @TSFI.NTP @S2 @S3
--
-- Using an expired access token should fail
testExpired :: C.Env -> Vector PublicKey -> IO ()
testExpired env pubKeys = do
  x <- runCreateAndValidate env pubKeys $ do
    u <- liftIO nextRandom
    t <- newToken @_ @U (TokenExpiresAfter 0) Nothing $ User u Nothing 100
    waitSeconds 1
    check t
  Left Expired @=? x

-- @END

testSignAndVerify :: C.Env -> Vector PublicKey -> IO ()
testSignAndVerify env pubKeys = do
  x <- runCreateAndValidate env pubKeys $ do
    u <- liftIO nextRandom
    t <- newToken @_ @U (TokenExpiresAfter defDuration) Nothing $ User u Nothing 100
    check t
  Right () @=? x

-- Helpers:

waitSeconds :: (MonadIO m) => Int -> m ()
waitSeconds n = liftIO $ threadDelay (n * 10 ^ (6 :: Int))
