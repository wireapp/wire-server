module API.Proxy where

import Testlib.Prelude

getGiphy :: (HasCallStack, MakesValue caller) => caller -> String -> [(String, String)] -> App Response
getGiphy caller path qparams = do
  req <- baseRequest caller WireProxy Unversioned ("/proxy/giphy/v1/gifs/" <> path)
  submit "GET" (req & addQueryParams qparams)
