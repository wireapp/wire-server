module Network.Federation.Util.DNS
    ( srvLookup
    ) where

import Imports
import Network.DNS
import Network.Federation.Util.Internal

-- | Looks up a SRV record given a domain, returning A(AAA) records with their
-- ports (ordered by priority and weight according to RFC 2782).
--
-- Example:
--
-- > import Network.DNS.Resolver
-- > import Network.Federation.Util
-- >
-- > main :: IO ()
-- > main = do
-- >   rs <- makeResolvSeed defaultResolvConf
-- >   x <- srvLookup "staging.zinfra.io" rs
srvLookup :: Text -> ResolvSeed -> IO (Maybe [(Domain, Word16)])
srvLookup = srvLookup' srvDefaultPrefix

srvDefaultPrefix :: Text
srvDefaultPrefix = "_wire-server"
