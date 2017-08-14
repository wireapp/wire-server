{-# LANGUAGE TemplateHaskell       #-}

module Brig.TURN where

import Brig.Types (TurnURI)
import Control.Lens
import Data.ByteString (ByteString)
import Data.List1
import Data.Word
import OpenSSL.EVP.Digest (Digest)
import System.Random.MWC (GenIO, createSystemRandom)

data Env = Env
    { _turnServers :: List1 TurnURI
    , _turnTTL     :: Word32
    , _turnSecret  :: ByteString
    , _turnSHA512  :: Digest
    , _turnPrng    :: GenIO
    }

makeLenses ''Env

newEnv :: Digest -> List1 TurnURI -> Word32 -> ByteString -> IO Env
newEnv sha512 srvs ttl secret = Env srvs ttl secret sha512 <$> createSystemRandom
