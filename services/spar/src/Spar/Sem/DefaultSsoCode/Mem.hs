{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.DefaultSsoCode.Mem where

import Imports
import Polysemy
import Polysemy.State (evalState, get, put)
import qualified SAML2.WebSSO as SAML
import Spar.Sem.DefaultSsoCode (DefaultSsoCode (..))

defaultSsoCodeToMem :: Sem (DefaultSsoCode ': r) a -> Sem r a
defaultSsoCodeToMem = (evalState (Nothing @(SAML.IdPId)) .) $
  reinterpret $ \case
    Get -> get
    Store ipi -> put $ Just ipi
    Delete -> put Nothing
