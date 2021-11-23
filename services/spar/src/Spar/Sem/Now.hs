module Spar.Sem.Now where

import Imports
import Polysemy
import qualified SAML2.WebSSO as SAML

data Now m a where
  Get :: Now m SAML.Time

makeSem ''Now

deriving instance Show (Now m a)

-- | Check a time against 'Now', checking if it's still alive (hasn't occurred yet.)
boolTTL ::
  Member Now r =>
  -- | The value to return if the TTL is expired
  a ->
  -- | The value to return if the TTL is alive
  a ->
  SAML.Time -> -- The time to check
  Sem r a
boolTTL f t time = do
  now <- get
  pure $ bool f t $ now <= time
