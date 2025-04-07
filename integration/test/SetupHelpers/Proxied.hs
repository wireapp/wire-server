{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}

-- | services needed to test /services/proxy (giphy, ...)
module SetupHelpers.Proxied where

{-
type GiphyPath = String
type GiphyApiKey = String

data GiphyAPI =
  "v1" :> "gifs" :> Capture "path" GiphyPath :> QueryParam "api_key" GiphyAPIKey :>
  Get '[JSON]
-}
