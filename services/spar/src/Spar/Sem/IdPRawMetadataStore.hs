module Spar.Sem.IdPRawMetadataStore where

import Imports
import Polysemy
import qualified SAML2.WebSSO as SAML

data IdPRawMetadataStore m a where
  Store :: SAML.IdPId -> Text -> IdPRawMetadataStore m ()
  Get :: SAML.IdPId -> IdPRawMetadataStore m (Maybe Text)
  Delete :: SAML.IdPId -> IdPRawMetadataStore m ()

deriving stock instance Show (IdPRawMetadataStore m a)

-- TODO(sandy): Inline this definition --- no TH
makeSem ''IdPRawMetadataStore
