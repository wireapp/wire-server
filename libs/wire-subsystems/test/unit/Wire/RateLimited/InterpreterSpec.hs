{-# LANGUAGE RecordWildCards #-}

module Wire.RateLimited.InterpreterSpec where

import Data.IP
import Data.Id
import Data.Misc
import Imports
import Polysemy
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.RateLimit
import Wire.RateLimit.Interpreter

defaultTestConfig :: Int -> TokenBucketConfig -> TokenBucketConfig -> TokenBucketConfig -> RateLimitConfig
defaultTestConfig maxRateLimitedKeys ipAddrLimit userLimit internalLimit =
  RateLimitConfig {ipv4CidrBlock = 32, ipv6CidrBlock = 128, ..}

spec :: Spec
spec = do
  describe "checkRateLimit" $ do
    prop "returns non-zero when rate limit is exceeded" $ \(key :: RateLimitKey) -> ioProperty $ do
      let tbConfig = TokenBucketConfig {burst = 1, inverseRate = 1_000_000}
      env <- newRateLimitEnv $ defaultTestConfig 1 tbConfig tbConfig tbConfig
      runM . interpretRateLimit env $ do
        wait1 <- checkRateLimit key
        wait2 <- checkRateLimit key
        wait3 <- checkRateLimit key
        pure $
          counterexample "wait1" (wait1 === 0)
            .&. counterexample "wait2" (wait2 =/= 0)
            .&. counterexample "wait3" (wait3 =/= 0)

    prop "rate limits are tracked per user" $ \(user1 :: UserId) (user2 :: UserId) -> ioProperty $ do
      let tbConfig = TokenBucketConfig {burst = 1, inverseRate = 1_000_000}
          key1 = RateLimitUser user1
          key2 = RateLimitUser user2
      env <- newRateLimitEnv $ defaultTestConfig 2 tbConfig tbConfig tbConfig
      runM . interpretRateLimit env $ do
        user1Wait1 <- checkRateLimit key1
        user1Wait2 <- checkRateLimit key1
        user2Wait1 <- checkRateLimit key2
        pure $
          counterexample "user1Wait1" (user1Wait1 === 0)
            .&. counterexample "user1Wait2" (user1Wait2 =/= 0)
            .&. counterexample "user2Wait1" (user2Wait1 === 0)

    prop "rate limits are tracked per IP" $ \(ip1 :: IpAddr) (ip2 :: IpAddr) -> ioProperty $ do
      let tbConfig = TokenBucketConfig {burst = 1, inverseRate = 1_000_000}
          key1 = RateLimitIp ip1
          key2 = RateLimitIp ip2
      env <- newRateLimitEnv $ defaultTestConfig 2 tbConfig tbConfig tbConfig
      runM . interpretRateLimit env $ do
        ip1Wait1 <- checkRateLimit key1
        ip1Wait2 <- checkRateLimit key1
        ip2Wait1 <- checkRateLimit key2
        pure $
          counterexample "ip1Wait1" (ip1Wait1 === 0)
            .&. counterexample "ip1Wait2" (ip1Wait2 =/= 0)
            .&. counterexample "ip2Wait1" (ip2Wait1 === 0)

    prop "applies different rate limits to IPAddresses, Users, and Internal usages" $ \(userId :: UserId) (ip :: IpAddr) -> ioProperty $ do
      let ipTBConfig = TokenBucketConfig {burst = 1, inverseRate = 1_000_000}
          userTBConfig = TokenBucketConfig {burst = 2, inverseRate = 1_000}
          internalTBConfig = TokenBucketConfig {burst = 3, inverseRate = 5_000}
          userKey = RateLimitUser userId
          ipKey = RateLimitIp ip
          internalKey = RateLimitInternal
      env <- newRateLimitEnv $ defaultTestConfig 2 ipTBConfig userTBConfig internalTBConfig
      runM . interpretRateLimit env $ do
        userWait1 <- checkRateLimit userKey
        userWait2 <- checkRateLimit userKey
        userWait3 <- checkRateLimit userKey

        ipWait1 <- checkRateLimit ipKey
        ipWait2 <- checkRateLimit ipKey

        -- Not enough delay for IP rate limit
        threadDelay $ fromIntegral userTBConfig.inverseRate
        userWait4 <- checkRateLimit userKey
        ipWait3 <- checkRateLimit ipKey

        internalWait1 <- checkRateLimit internalKey
        internalWait2 <- checkRateLimit internalKey
        internalWait3 <- checkRateLimit internalKey
        internalWait4 <- checkRateLimit internalKey
        pure $
          counterexample "userWait1" (userWait1 === 0)
            .&. counterexample "userWait2" (userWait2 === 0)
            .&. counterexample "userWait3" (userWait3 =/= 0)
            .&. counterexample "ipWait1" (ipWait1 === 0)
            .&. counterexample "ipWait2" (ipWait2 =/= 0)
            .&. counterexample "userWait4" (userWait4 === 0)
            .&. counterexample "ipWait3" (ipWait3 =/= 0)
            .&. counterexample "internalWait1" (internalWait1 === 0)
            .&. counterexample "internalWait2" (internalWait2 === 0)
            .&. counterexample "internalWait3" (internalWait3 === 0)
            .&. counterexample "internalWait4" (internalWait4 =/= 0)

    prop "allows bursts" $ \(key :: RateLimitKey) -> ioProperty $ do
      let tbConfig = TokenBucketConfig {burst = 2, inverseRate = 1_000_000}
      env <- newRateLimitEnv $ defaultTestConfig 1 tbConfig tbConfig tbConfig
      runM . interpretRateLimit env $ do
        wait1 <- checkRateLimit key
        wait2 <- checkRateLimit key
        wait3 <- checkRateLimit key
        pure $
          counterexample "wait1" (wait1 === 0)
            .&. counterexample "wait2" (wait2 === 0)
            .&. counterexample "wait3" (wait3 =/= 0)

    prop "recovers after waiting long enough, but doesn't allow bursts immediately" $ \(key :: RateLimitKey) -> ioProperty $ do
      -- The inverseRate here decides how long a test takes, reducing it a lot
      -- might make the tests flaky. 10ms _should_ be long enough to do three
      -- check operations.
      let tbConfig = TokenBucketConfig {burst = 2, inverseRate = 10_000}
      env <- newRateLimitEnv $ defaultTestConfig 1 tbConfig tbConfig tbConfig
      runM . interpretRateLimit env $ do
        wait1 <- checkRateLimit key
        wait2 <- checkRateLimit key
        wait3 <- checkRateLimit key
        embed $ threadDelay (fromIntegral wait3)
        wait4 <- checkRateLimit key
        wait5 <- checkRateLimit key
        pure $
          counterexample "wait1" (wait1 === 0)
            .&. counterexample "wait2" (wait2 === 0)
            .&. counterexample "wait3" (wait3 =/= 0)
            .&. counterexample "wait4" (wait4 === 0)
            .&. counterexample "wait5" (wait5 =/= 0)

    prop "waiting for (inverseRate * burst) should allow bursts again" $ \(key :: RateLimitKey) -> ioProperty $ do
      let tbConfig = TokenBucketConfig {burst = 2, inverseRate = 1000}
      env <- newRateLimitEnv $ defaultTestConfig 1 tbConfig tbConfig tbConfig
      runM . interpretRateLimit env $ do
        wait1 <- checkRateLimit key
        wait2 <- checkRateLimit key
        wait3 <- checkRateLimit key
        embed $ threadDelay (fromIntegral (tbConfig.burst * tbConfig.inverseRate))
        wait4 <- checkRateLimit key
        wait5 <- checkRateLimit key
        pure $
          counterexample "wait1" (wait1 === 0)
            .&. counterexample "wait2" (wait2 === 0)
            .&. counterexample "wait3" (wait3 =/= 0)
            .&. counterexample "wait4" (wait4 === 0)
            .&. counterexample "wait5" (wait5 === 0)

    prop "limits memory usage by evicting least recently used RateLimitKey" $
      \(key1 :: RateLimitKey) (key2 :: RateLimitKey) (key3 :: RateLimitKey) ->
        (key1 /= key2 && key2 /= key3 && key1 /= key3) ==> ioProperty $ do
          let tbConfig = TokenBucketConfig {burst = 1, inverseRate = 1_000_000}
          env <- newRateLimitEnv $ defaultTestConfig 2 tbConfig tbConfig tbConfig
          runM . interpretRateLimit env $ do
            key1Wait1 <- checkRateLimit key1
            key2Wait1 <- checkRateLimit key2
            key1Wait2 <- checkRateLimit key1
            key3Wait1 <- checkRateLimit key3
            -- Now key2 is evicted, so its allowed to bypass the rate limit, but
            -- key1 should still be rate limited.
            key1Wait3 <- checkRateLimit key1
            key2Wait2 <- checkRateLimit key2
            -- Now key3 is evicted, so its allowed to bypass the rate limit
            key3Wait2 <- checkRateLimit key3
            pure $
              counterexample "key1Wait1" (key1Wait1 === 0)
                .&. counterexample "key2Wait1" (key2Wait1 === 0)
                .&. counterexample "key1Wait2" (key1Wait2 =/= 0)
                .&. counterexample "key3Wait1" (key3Wait1 === 0)
                .&. counterexample "key1Wait3" (key1Wait3 =/= 0)
                .&. counterexample "key2Wait2" (key2Wait2 === 0)
                .&. counterexample "key3Wait2" (key3Wait2 === 0)

    -- Too much work to write a property test
    it "applies rate limits to IPv4 CIDR blocks" $ do
      let range1_ip1 = RateLimitIp . IpAddr $ read @IP "10.0.0.1"
          range1_ip2 = RateLimitIp . IpAddr $ read @IP "10.0.0.2"
          range2_ip1 = RateLimitIp . IpAddr $ read @IP "10.0.1.1"
      let tbConfig = TokenBucketConfig {burst = 1, inverseRate = 1_000_000}
      env <-
        newRateLimitEnv $
          RateLimitConfig
            { ipAddrLimit = tbConfig,
              userLimit = tbConfig,
              internalLimit = tbConfig,
              ipv4CidrBlock = 24,
              ipv6CidrBlock = 128,
              maxRateLimitedKeys = 2
            }
      runM . interpretRateLimit env $ do
        range1_ip1_wait <- checkRateLimit range1_ip1
        range1_ip2_wait <- checkRateLimit range1_ip2
        range2_ip1_wait <- checkRateLimit range2_ip1
        liftIO $ do
          range1_ip1_wait `shouldBe` 0
          range1_ip2_wait `shouldNotBe` 0
          range2_ip1_wait `shouldBe` 0

    -- Too much work to write a property test
    it "applies rate limits to IPv6 CIDR blocks" $ do
      let range1_ip1 = RateLimitIp . IpAddr $ read @IP "fc00::1"
          range1_ip2 = RateLimitIp . IpAddr $ read @IP "fc00::2"
          range2_ip1 = RateLimitIp . IpAddr $ read @IP "fc00::1:0:0:0:1"
      let tbConfig = TokenBucketConfig {burst = 1, inverseRate = 1_000_000}
      env <-
        newRateLimitEnv $
          RateLimitConfig
            { ipAddrLimit = tbConfig,
              userLimit = tbConfig,
              internalLimit = tbConfig,
              ipv4CidrBlock = 32,
              ipv6CidrBlock = 64,
              maxRateLimitedKeys = 10
            }
      runM . interpretRateLimit env $ do
        range1_ip1_wait <- checkRateLimit range1_ip1
        range1_ip2_wait <- checkRateLimit range1_ip2
        range2_ip1_wait <- checkRateLimit range2_ip1
        liftIO $ do
          range1_ip1_wait `shouldBe` 0
          range1_ip2_wait `shouldNotBe` 0
          range2_ip1_wait `shouldBe` 0

  describe "tryRateLimit" $ do
    prop "executes an operation only when allowed by rate limit" $ \(key :: RateLimitKey) -> ioProperty $ do
      let tbConfig = TokenBucketConfig {burst = 1, inverseRate = 1_000_000}
      env <- newRateLimitEnv $ defaultTestConfig 1 tbConfig tbConfig tbConfig
      runM . interpretRateLimit env $ do
        try1 <- tryRateLimited key $ pure @_ @String "try 1"
        try2 <- tryRateLimited key $ pure @_ @String "try 2"
        pure $
          try1 === Right "try 1" .&. try2 =/= Right "try 2"
