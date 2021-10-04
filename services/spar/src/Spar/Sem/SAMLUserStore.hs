module Spar.Sem.SAMLUserStore where

import Data.Id
import Imports
import Polysemy
import qualified SAML2.WebSSO as SAML

data SAMLUserStore m a where
  Insert :: SAML.UserRef -> UserId -> SAMLUserStore m ()
  Get :: SAML.UserRef -> SAMLUserStore m (Maybe UserId)
  GetAnyByIssuer :: SAML.Issuer -> SAMLUserStore m (Maybe UserId)
  GetSomeByIssuer :: SAML.Issuer -> SAMLUserStore m [(SAML.UserRef, UserId)]
  DeleteByIssuer :: SAML.Issuer -> SAMLUserStore m ()
  Delete :: UserId -> SAML.UserRef -> SAMLUserStore m ()

-- TODO(sandy): Inline this definition --- no TH
makeSem ''SAMLUserStore
