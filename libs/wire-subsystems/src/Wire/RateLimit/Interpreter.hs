{-# LANGUAGE RecordWildCards #-}

module Wire.RateLimit.Interpreter where

import Control.Concurrent.TokenBucket
import Data.Aeson
import Data.IP
import Data.Id
import Data.LruCache
import Data.LruCache qualified as LruCache
import Data.Misc
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import Wire.RateLimit

data TokenBucketConfig = TokenBucketConfig
  { -- | Burst operations allowed
    burst :: Word64,
    -- | Avg. microseconds per operation, setting this to 0 will effectively
    -- remove any rate limiting.
    inverseRate :: Word64
  }
  deriving (Show, Eq, Generic)

instance FromJSON TokenBucketConfig

data RateLimitConfig = RateLimitConfig
  { ipAddrLimit :: TokenBucketConfig,
    userLimit :: TokenBucketConfig,
    internalLimit :: TokenBucketConfig,
    -- | Must be between 0 and 32 (both inclusive). Decides how many bits of the
    -- IPv4 address are to be considered as key for rate limiting. Setting it to
    -- 32 will rate limit each IPv4 address separately.
    ipv4CidrBlock :: Int,
    -- | Must be betweeen 0 and 128 (both inclusive). Decides how many bits of
    -- the IPv6 address are to be considered as key for rate limiting. Setting
    -- it to 128 will rate limit each IPv6 address seperately. It is recommended
    -- to set this to at least 64 as most popular ISPs allocate that to each
    -- user.
    ipv6CidrBlock :: Int,
    -- | Maximum size of RateLimitKey -> TokenBucket map. When full the least
    -- recently used keys are dropped first.
    maxRateLimitedKeys :: Int,
    ipAddressExceptions :: [IpAddrRange]
  }
  deriving (Show, Eq, Generic)

instance FromJSON RateLimitConfig

data RateLimitEnv = RateLimitEnv
  { tokenBucketsRef :: IORef (LruCache RateLimitKey TokenBucket),
    config :: RateLimitConfig
  }

-- | 'newRateLimitEnv size config'
--
-- The size parameter is the maximum number of 'RateLimitKey's tracked. Once
-- this number is reached, least recently used keys are evicted. This limits the
-- memory usage of rate limiting.
newRateLimitEnv :: RateLimitConfig -> IO RateLimitEnv
newRateLimitEnv config = do
  tokenBucketsRef <- newIORef $ LruCache.empty config.maxRateLimitedKeys
  pure $ RateLimitEnv {..}

interpretRateLimit ::
  forall r a.
  ( Member (Embed IO) r,
    Member (Error RateLimitExceeded) r,
    Member Log.TinyLog r
  ) =>
  RateLimitEnv ->
  Sem (RateLimit ': r) a ->
  Sem r a
interpretRateLimit env = interpretH $ \case
  CheckRateLimit key ->
    pureT =<< embed (checkRateLimitImpl env key)
  DoRateLimited key action -> doRateLimitedImpl env key action

doRateLimitedImpl ::
  ( Member (Embed IO) r,
    Member (Error RateLimitExceeded) r,
    Member Log.TinyLog r,
    Functor f
  ) =>
  RateLimitEnv ->
  RateLimitKey ->
  m a ->
  Sem (WithTactics e f m r) (f a)
doRateLimitedImpl env key action = do
  retryWait <- embed (checkRateLimitImpl env key)
  if retryWait == 0
    then runTSimple action
    else do
      Log.info $
        Log.msg (Log.val "Operation rate limited")
          . logKey
          . Log.field "wait_time" retryWait
      throw $ RateLimitExceeded retryWait
  where
    logKey :: Log.Msg -> Log.Msg
    logKey = case key of
      RateLimitIp ipAddr -> Log.field "ip_address" (show ipAddr)
      RateLimitUser uid -> Log.field "user" (idToText uid)
      RateLimitProvider pid -> Log.field "provider" (idToText pid)
      RateLimitInternal -> Log.field "internal" ("true" :: ByteString)

checkRateLimitImpl :: RateLimitEnv -> RateLimitKey -> IO Word64
checkRateLimitImpl env origKey = do
  let isExcepted = withIpAddress origKey False $ \ip ->
        any (addrMatchesRange ip) env.config.ipAddressExceptions
  if isExcepted
    then pure 0
    else do
      let maskedKey = withIpAddress origKey origKey (RateLimitIp . maskIp env.config)
      -- Seems unnecessary in most cases, but avoids IO in the
      -- 'atomicModifyIORef'
      newBucket <- newTokenBucket
      bucket <- atomicModifyIORef env.tokenBucketsRef $ \tokenBuckets ->
        case LruCache.lookup maskedKey tokenBuckets of
          Nothing -> do
            (LruCache.insert maskedKey newBucket tokenBuckets, newBucket)
          Just (bucket, newBuckets) ->
            (newBuckets, bucket)
      let tokenBucketConfig = case maskedKey of
            RateLimitIp {} -> env.config.ipAddrLimit
            RateLimitUser {} -> env.config.userLimit
            RateLimitProvider {} -> env.config.userLimit
            RateLimitInternal -> env.config.internalLimit
      tokenBucketTryAlloc1 bucket tokenBucketConfig.burst tokenBucketConfig.inverseRate

maskIp :: RateLimitConfig -> IpAddr -> IpAddr
maskIp RateLimitConfig {ipv4CidrBlock} ((IpAddr (IPv4 ip))) =
  IpAddr . IPv4 $
    ip `masked` intToMask ipv4CidrBlock
maskIp RateLimitConfig {ipv6CidrBlock} ((IpAddr (IPv6 ip))) =
  IpAddr . IPv6 $
    ip `masked` intToMask ipv6CidrBlock
