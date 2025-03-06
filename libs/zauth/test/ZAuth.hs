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
import Control.Lens
import Data.ByteString.Conversion
import Data.UUID.V4
import Data.ZAuth.Creation as C
import Data.ZAuth.Token
import Data.ZAuth.Validation as V
import Imports
import Polysemy
import Sodium.Crypto.Sign
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

runCreate :: C.Env -> Sem '[ZAuthCreation, Embed IO] a -> IO a
runCreate env = runM . C.interpretZAuthCreation env

tests :: IO TestTree
tests = do
  (p1, s1) <- newKeyPair
  (p2, s2) <- newKeyPair
  (p3, s3) <- newKeyPair
  z1 <- C.mkEnv 1 s1 [s2, s3]
  z2 <- C.mkEnv 2 s1 [s2, s3]
  z3 <- C.mkEnv 3 s1 [s2, s3]
  let v = V.mkEnv p1 [p2, p3]
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
          [ testCase "expired" (runCreate z1 $ testExpired v),
            testCase "not expired" (runCreate z2 $ testNotExpired v),
            testCase "signed access-token is valid" (runCreate z3 $ testSignAndVerify v)
          ],
        testGroup
          "Various"
          [testCase "random device ids" (runCreate z1 testRandDevIds)]
      ]

defDuration :: Integer
defDuration = 1

testUserIsNotLegalHoldUser :: Token (User LHUser) -> Property
testUserIsNotLegalHoldUser t = fromByteString @(Token (User ActualUser)) (toByteString' t) === Nothing

testUserIsNotLegalHoldUser' :: Token (User ActualUser) -> Property
testUserIsNotLegalHoldUser' t = fromByteString @(Token (User LHUser)) (toByteString' t) === Nothing

testDecEncAccessToken :: Token (Access ActualUser) -> Property
testDecEncAccessToken t = fromByteString (toByteString' t) === Just t

testDecEncUserToken :: Token (User ActualUser) -> Property
testDecEncUserToken t = fromByteString (toByteString' t) === Just t

testDecEncLegalHoldUserToken :: Token (User LHUser) -> Property
testDecEncLegalHoldUserToken t = fromByteString (toByteString' t) === Just t

testDecEncLegalHoldAccessToken :: Token (Access LHUser) -> Property
testDecEncLegalHoldAccessToken t = fromByteString (toByteString' t) === Just t

testNotExpired :: (Member (Embed IO) r, Member ZAuthCreation r) => V.Env -> Sem r ()
testNotExpired p = do
  u <- liftIO nextRandom
  t <- userToken defDuration u Nothing 100
  x <- liftIO $ runValidate p $ check t
  liftIO $ assertBool "testNotExpired: validation failed" (isRight x)

-- The testExpired test conforms to the following testing standards:
-- @SF.Channel @TSFI.RESTfulAPI @TSFI.NTP @S2 @S3
--
-- Using an expired access token should fail
testExpired :: (Member (Embed IO) r, Member ZAuthCreation r) => V.Env -> Sem r ()
testExpired p = do
  u <- liftIO nextRandom
  t <- userToken 0 u Nothing 100
  waitSeconds 1
  x <- liftIO $ runValidate p $ check t
  liftIO $ Left Expired @=? x

-- @END

testSignAndVerify :: (Member (Embed IO) r, Member ZAuthCreation r) => V.Env -> Sem r ()
testSignAndVerify p = do
  u <- liftIO nextRandom
  t <- userToken defDuration u Nothing 100
  x <- liftIO $ runValidate p $ check t
  liftIO $ assertBool "testSignAndVerify: validation failed" (isRight x)

testRandDevIds :: (Member (Embed IO) r, Member ZAuthCreation r) => Sem r ()
testRandDevIds = do
  u <- liftIO nextRandom
  t1 <- view body <$> accessToken1 defDuration u Nothing
  t2 <- view body <$> accessToken1 defDuration u Nothing
  liftIO $ assertBool "unexpected: Same device ID." (t1 ^. connection /= t2 ^. connection)

-- Helpers:

waitSeconds :: (MonadIO m) => Int -> m ()
waitSeconds n = liftIO $ threadDelay (n * 10 ^ (6 :: Int))
