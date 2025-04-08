module API.Proxy where

import Testlib.Prelude

getGiphy :: (HasCallStack, MakesValue caller) => caller -> App Response
getGiphy caller = do
  req <- baseRequest caller WireProxy Unversioned "/proxy/giphy/v1/gifs/search"
  submit "GET" (req & addQueryParams [("q", "monday"), ("limit", "100"), ("offset", "0")])

getGiphyNotFound :: (HasCallStack, MakesValue caller) => caller -> App Response
getGiphyNotFound caller = do
  req <- baseRequest caller WireProxy Unversioned "/proxy/giphy/v1/gifs/search/sorch"
  submit "GET" req

getGiphyBadRequest :: (HasCallStack, MakesValue caller) => caller -> App Response
getGiphyBadRequest caller = do
  req <- baseRequest caller WireProxy Unversioned "/proxy/giphy/v1/gifs/search?limit=true"
  submit "GET" req
