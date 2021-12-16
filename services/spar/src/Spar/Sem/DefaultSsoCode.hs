module Spar.Sem.DefaultSsoCode where

import Imports
import Polysemy
import qualified SAML2.WebSSO as SAML

data DefaultSsoCode m a where
  Get :: DefaultSsoCode m (Maybe SAML.IdPId)
  Store :: SAML.IdPId -> DefaultSsoCode m ()
  Delete :: DefaultSsoCode m ()

deriving instance Show (DefaultSsoCode m a)

makeSem ''DefaultSsoCode
