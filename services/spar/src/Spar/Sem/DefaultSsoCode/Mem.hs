{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.DefaultSsoCode.Mem where

import Imports
import Polysemy
import Polysemy.State (get, put, runState)
import qualified SAML2.WebSSO as SAML
import Spar.Sem.DefaultSsoCode (DefaultSsoCode (..))

defaultSsoCodeToMem :: Sem (DefaultSsoCode ': r) a -> Sem r (Maybe SAML.IdPId, a)
defaultSsoCodeToMem = (runState Nothing .) $
  reinterpret $ \case
    Get -> get
    Store ipi -> put $ Just ipi
    Delete -> put Nothing
