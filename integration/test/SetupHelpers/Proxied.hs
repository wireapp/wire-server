{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}

-- | services needed to test /services/proxy (giphy, ...)
module SetupHelpers.Proxied where

import Servant
import Testlib.Prelude

type GiphyApiKey = String

data GiphyResponse = GiphyResponse
  { apiKey :: GiphyApiKey,
    q :: String,
    limit :: Int,
    offset :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON GiphyResponse

instance ToJSON GiphyResponse

type GiphyAPI =
  "v1"
    :> "gifs"
    :> "search"
    :> QueryParam "api_key" GiphyApiKey
    :> QueryParam "q" String
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Get '[JSON] GiphyResponse

-- TODO: Does this belong here? Wouldn't it be better if the tests could define
-- their mocks themselves?
giphyAPI :: Server GiphyAPI
giphyAPI = undefined
