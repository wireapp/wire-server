{-# LANGUAGE CPP #-}

module SAML2.WebSSO.Servant.CPP where

import Servant qualified

type MkLink endpoint = Servant.MkLink endpoint
#if MIN_VERSION_servant(0,14,0)
                       Servant.Link
#endif
