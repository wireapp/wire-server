{-# LANGUAGE OverloadedStrings #-}

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

module ZAuth
  ( tests,
  )
where

import Arbitraries ()
import Control.Lens
import Data.ByteString.Conversion
import Data.UUID.V4
import Data.ZAuth.Creation as C
import Data.ZAuth.Token
import Data.ZAuth.Validation as V
import Imports
import Sodium.Crypto.Sign
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: IO TestTree
tests = do
  (p1, s1) <- newKeyPair
  (p2, s2) <- newKeyPair
  (p3, s3) <- newKeyPair
  z <- C.mkEnv s1 [s2, s3]
  let v = V.mkEnv p1 [p2, p3]
  return $
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
          [ testCase "expired" (runCreate z 1 $ testExpired v),
            testCase "not expired" (runCreate z 2 $ testNotExpired v),
            testCase "signed access-token is valid" (runCreate z 3 $ testSignAndVerify v)
          ],
        testGroup
          "Various"
          [testCase "random device ids" (runCreate z 1 testRandDevIds)]
      ]

defDuration :: Integer
defDuration = 1

testUserIsNotLegalHoldUser :: Token LegalHoldUser -> Bool
testUserIsNotLegalHoldUser t = fromByteString @(Token User) (toByteString' t) == Nothing

testUserIsNotLegalHoldUser' :: Token User -> Bool
testUserIsNotLegalHoldUser' t = fromByteString @(Token LegalHoldUser) (toByteString' t) == Nothing

testDecEncAccessToken :: Token Access -> Bool
testDecEncAccessToken t = fromByteString (toByteString' t) == Just t

testDecEncUserToken :: Token User -> Bool
testDecEncUserToken t = fromByteString (toByteString' t) == Just t

testDecEncLegalHoldUserToken :: Token LegalHoldUser -> Bool
testDecEncLegalHoldUserToken t = fromByteString (toByteString' t) == Just t

testDecEncLegalHoldAccessToken :: Token LegalHoldAccess -> Bool
testDecEncLegalHoldAccessToken t = fromByteString (toByteString' t) == Just t

testNotExpired :: V.Env -> Create ()
testNotExpired p = do
  u <- liftIO nextRandom
  t <- userToken defDuration u 100
  x <- liftIO $ runValidate p $ check t
  liftIO $ assertBool "testNotExpired: validation failed" (isRight x)

testExpired :: V.Env -> Create ()
testExpired p = do
  u <- liftIO nextRandom
  t <- userToken 0 u 100
  waitSeconds 1
  x <- liftIO $ runValidate p $ check t
  liftIO $ Left Expired @=? x

testSignAndVerify :: V.Env -> Create ()
testSignAndVerify p = do
  u <- liftIO nextRandom
  t <- userToken defDuration u 100
  x <- liftIO $ runValidate p $ check t
  liftIO $ assertBool "testSignAndVerify: validation failed" (isRight x)

testRandDevIds :: Create ()
testRandDevIds = do
  u <- liftIO nextRandom
  t1 <- (view body) <$> accessToken1 defDuration u
  t2 <- (view body) <$> accessToken1 defDuration u
  liftIO $ assertBool "unexpected: Same device ID." (t1 ^. connection /= t2 ^. connection)

-- Helpers:

waitSeconds :: (MonadIO m) => Int -> m ()
waitSeconds n = liftIO $ threadDelay (n * 10 ^ (6 :: Int))
