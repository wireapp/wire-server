{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Journal.Types
    ( CorrelationId
    , _RequestId
    )
where

import Data.Hashable                 (Hashable)
import Data.Id                       (RequestId (..))
import Data.Monoid
import Data.Profunctor
import Data.ProtocolBuffers.Internal
import GHC.Generics                  (Generic)

import Prelude


-- $setup
-- >>> :m + Control.Lens

-- | An identifier to correlate events, typically a 'RequestId'
newtype CorrelationId a = CorrelationId
    { fromCorrelationId :: a
    } deriving (Eq, Show, Read, Generic, Hashable)

instance EncodeWire a => EncodeWire (CorrelationId a) where
    encodeWire t = encodeWire t . fromCorrelationId

instance DecodeWire a => DecodeWire (CorrelationId a) where
    decodeWire = fmap CorrelationId . decodeWire

instance Monoid a => Monoid (CorrelationId a) where
    mempty = CorrelationId mempty
    mappend (CorrelationId a) (CorrelationId b) = CorrelationId $ mappend a b

-- | A 'CorrelationId RequestId' is isomorphic to a 'RequestId'. We can
-- construct the former from the latter:
--
-- >>> view (from _RequestId) mempty
-- CorrelationId {fromCorrelationId = RequestId {unRequestId = "N/A"}}
--
_RequestId
    :: (Profunctor p, Functor f)
    => p RequestId (f RequestId)
    -> p (CorrelationId RequestId) (f (CorrelationId RequestId))
_RequestId = dimap fromCorrelationId (fmap CorrelationId)
{-# INLINE _RequestId #-}
