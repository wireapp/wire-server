module Data.Journal.Internal
    ( flens
    , toTimestamp
    , fromTimestamp
    )
where

import Control.Applicative
import Data.ProtocolBuffers
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Prelude


-- | A lens into a field of a 'Data.ProtocolBuffers' record, bypassing the
-- 'Data.ProtocolBuffers.Field' metadata.
flens :: (Functor f, HasField a)
      => (s -> a)
      -> (s -> a -> b)
      -> (FieldType a -> f (FieldType a))
      -> s
      -> f b
flens sa sab afa s = sab s <$> field afa (sa s)
{-# INLINE flens #-}

toTimestamp :: Integral a => UTCTime -> a
toTimestamp = round . utcTimeToPOSIXSeconds
{-# INLINE toTimestamp #-}

fromTimestamp :: Integral a => a -> UTCTime
fromTimestamp = posixSecondsToUTCTime . fromIntegral
{-# INLINE fromTimestamp #-}
