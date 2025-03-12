{-# LANGUAGE CPP #-}

module SAML2.WebSSO.Servant.CPP where

import Servant qualified

-- TODO: The Servant version is guaranteed by our Nix env, now. We should
-- inline this to avoid confusions.
type MkLink endpoint = Servant.MkLink endpoint
#if MIN_VERSION_servant(0,14,0)
                       Servant.Link
#endif
