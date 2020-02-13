module Brig.TURN where

import Brig.Types (TurnURI)
import Control.Lens
import Data.List1
import Imports
import OpenSSL.EVP.Digest (Digest)
import System.Random.MWC (GenIO, createSystemRandom)

data Env
  = Env
      { _turnServers :: List1 TurnURI,
        _turnTokenTTL :: Word32,
        _turnConfigTTL :: Word32,
        _turnSecret :: ByteString,
        _turnSHA512 :: Digest,
        _turnPrng :: GenIO
      }

makeLenses ''Env

newEnv :: Digest -> List1 TurnURI -> Word32 -> Word32 -> ByteString -> IO Env
newEnv sha512 srvs tTTL cTTL secret = Env srvs tTTL cTTL secret sha512 <$> createSystemRandom
