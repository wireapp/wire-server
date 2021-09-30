module Spar.Sem.SAML2 where

import Data.Id (TeamId)
import Data.String.Conversions (SBS, ST)
import Data.Time (NominalDiffTime)
import GHC.TypeLits (KnownSymbol)
import Imports hiding (log)
import Polysemy
import SAML2.WebSSO
import URI.ByteString (URI)

data SAML2 m a where
  AuthReq
      :: NominalDiffTime
      -> m Issuer -> IdPId -> SAML2 m (FormRedirect AuthnRequest)
  AuthResp
      :: Maybe TeamId
      -> m Issuer -> m URI -> (AuthnResponse -> AccessVerdict -> m resp) -> AuthnResponseBody -> SAML2 m resp
  Meta :: ST -> m Issuer -> m URI -> SAML2 m SPMetadata
  ToggleCookie ::
    KnownSymbol name =>
    SBS ->
    Maybe (ST, NominalDiffTime) ->
    SAML2 m (SimpleSetCookie name)

-- TODO(sandy): Inline this definition --- no TH
makeSem ''SAML2

