module Spar.Sem.SAMLUser where

import Data.Id
import Polysemy
import Imports
import qualified SAML2.WebSSO as SAML

data SAMLUser m a where
    Insert          :: SAML.UserRef -> UserId -> SAMLUser m ()
    Get             :: SAML.UserRef -> SAMLUser m (Maybe UserId)
    GetAnyByIssuer  :: SAML.Issuer -> SAMLUser m (Maybe UserId)
    GetSomeByIssuer :: SAML.Issuer -> SAMLUser m [(SAML.UserRef, UserId)]
    DeleteByIssuer  :: SAML.Issuer -> SAMLUser m ()
    Delete          :: UserId -> SAML.UserRef -> SAMLUser m ()

-- TODO(sandy): Inline this definition --- no TH
makeSem ''SAMLUser




