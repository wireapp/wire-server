{-# LANGUAGE OverloadedStrings #-}

module ZAuth (tests) where

import Arbitraries ()
import Control.Concurrent (threadDelay)
import Control.Error
import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString.Conversion
import Data.UUID.V4
import Data.ZAuth.Token
import Data.ZAuth.Creation as C
import Data.ZAuth.Validation as V
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
    return $ testGroup "ZAuth"
        [ testGroup "Parsing"
            [ testProperty "decode . encode == id [access]" testDecEncAccessToken
            , testProperty "decode . encode == id [user]" testDecEncUserToken
            ]
        , testGroup "Signing and Verifying"
            [ testCase "expired"                      (runCreate z 1 $ testExpired v)
            , testCase "not expired"                  (runCreate z 2 $ testNotExpired v)
            , testCase "signed access-token is valid" (runCreate z 3 $ testSignAndVerify v)
            ]
        , testGroup "Various"
            [ testCase "random device ids" (runCreate z 1 testRandDevIds) ]
        ]

defDuration :: Integer
defDuration = 1

testDecEncAccessToken :: Token Access -> Bool
testDecEncAccessToken t = fromByteString (toByteString' t) == Just t

testDecEncUserToken :: Token User -> Bool
testDecEncUserToken t = fromByteString (toByteString' t) == Just t

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
    u  <- liftIO nextRandom
    t1 <- (view body) <$> accessToken1 defDuration u
    t2 <- (view body) <$> accessToken1 defDuration u
    liftIO $ assertBool "unexpected: Same device ID." (t1^.connection /= t2^.connection)

-- Helpers:

waitSeconds :: (MonadIO m) => Int -> m ()
waitSeconds n = liftIO $ threadDelay (n * 10^(6 :: Int))
