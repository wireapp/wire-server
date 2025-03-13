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
import Data.ZAuth.CryptoSign (CryptoSign, runCryptoSign)
import Data.ZAuth.Token
import Data.ZAuth.Validation as V
import Imports
import Polysemy
import Sodium.Crypto.Sign
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Wire.Sem.Now (Now)
import Wire.Sem.Now.IO

tests :: IO TestTree
tests = do
  (p1, s1) <- newKeyPair
  (p2, s2) <- newKeyPair
  (p3, s3) <- newKeyPair
  let z1 = C.SigningKey 1 s1
      z2 = C.SigningKey 2 s2
      z3 = C.SigningKey 3 s3
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

runEffects :: Sem '[CryptoSign, Now, Embed IO] a -> IO a
runEffects = runM . nowToIO . runCryptoSign

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

testNotExpired :: C.SigningKey -> Vector PublicKey -> IO ()
testNotExpired signingKey pubKeys = runEffects $ do
  u <- liftIO nextRandom
  t <- newToken @U signingKey (TokenExpiresAfter defDuration) Nothing $ User u Nothing 100
  x <- check pubKeys t
  liftIO $ Right () @=? x

-- The testExpired test conforms to the following testing standards:
-- @SF.Channel @TSFI.RESTfulAPI @TSFI.NTP @S2 @S3
--
-- Using an expired access token should fail
testExpired :: C.SigningKey -> Vector PublicKey -> IO ()
testExpired signingKey pubKeys = runEffects $ do
  u <- liftIO nextRandom
  t <- newToken @U signingKey (TokenExpiresAfter 0) Nothing $ User u Nothing 100
  waitSeconds 1
  x <- check pubKeys t
  liftIO $ Left Expired @=? x

-- @END

testSignAndVerify :: C.SigningKey -> Vector PublicKey -> IO ()
testSignAndVerify signingKey pubKeys = runEffects $ do
  u <- liftIO nextRandom
  t <- newToken @U signingKey (TokenExpiresAfter defDuration) Nothing $ User u Nothing 100
  x <- check pubKeys t
  liftIO $ Right () @=? x

-- Helpers:

waitSeconds :: (MonadIO m) => Int -> m ()
waitSeconds n = liftIO $ threadDelay (n * 10 ^ (6 :: Int))
