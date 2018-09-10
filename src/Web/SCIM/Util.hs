{-# LANGUAGE TemplateHaskell #-}

-- | A utility module for the library.
module Web.SCIM.Util
  ( relativeUri
  ) where

import Language.Haskell.TH.Quote
import Network.URI
import Network.URI.Static () -- for the Lift URI instance

-- | A quasiquoter for parsing relative URIs at compile time, similar to 'uri'.
--
-- PR pending: <https://github.com/snakamura/network-uri-static/pull/3>
relativeUri :: QuasiQuoter
relativeUri = QuasiQuoter
  { quoteExp = \uri -> case parseRelativeReference uri of
        Nothing -> fail ("Invalid URI: " ++ uri)
        Just x  -> [| x |]
  , quotePat  = error "relativeUri: quotePat not implemented"
  , quoteType = error "relativeUri: quoteType not implemented"
  , quoteDec  = error "relativeUri: quoteDec not implemented"
  }
