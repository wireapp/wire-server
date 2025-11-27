module Wire.LegalHoldStore.Env where

import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Misc
import Imports
import Network.HTTP.Client qualified as Http

data LegalHoldEnv = LegalHoldEnv
  { makeVerifiedRequest :: Fingerprint Rsa -> HttpsUrl -> (Http.Request -> Http.Request) -> IO (Http.Response LC8.ByteString),
    makeVerifiedRequestFreshManager :: Fingerprint Rsa -> HttpsUrl -> (Http.Request -> Http.Request) -> IO (Http.Response LC8.ByteString)
  }
