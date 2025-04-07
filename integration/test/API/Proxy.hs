module API.Proxy where

import Testlib.Prelude

getGiphy :: (HasCallStack, MakesValue caller) => caller -> App Response
getGiphy caller = do
  req <- baseRequest caller WireProxy Versioned "/proxy/giphy/v1/gifs/search"
  submit "GET" (req & addQueryParams [("q", "monday"), ("limit", "100"), ("offset", "0")])
