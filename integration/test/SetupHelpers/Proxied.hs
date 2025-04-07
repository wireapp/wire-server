{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}

-- | services needed to test /services/proxy (giphy, ...)
module SetupHelpers.Proxied where

import Servant
import Testlib.Prelude

type GiphyPath = String

type GiphyApiKey = String

type GiphyAPI =
  "v1"
    :> "gifs"
    :> "search"
    :> QueryParam "api_key" GiphyApiKey
    :> QueryParam "q" String
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] NoContent

-- TODO: Does this belong here? Wouldn't it be better if the tests could define
-- their mocks themselves?
giphyAPI :: Server GiphyAPI
giphyAPI = undefined
