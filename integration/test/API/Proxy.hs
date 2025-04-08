module API.Proxy where

import Testlib.Prelude

getGiphy :: (HasCallStack, MakesValue caller) => caller -> String -> [(String, String)] -> App Response
getGiphy = callProxy "giphy/v1/gifs/"

getYoutube :: (HasCallStack, MakesValue caller) => caller -> String -> [(String, String)] -> App Response
getYoutube = callProxy "youtube/v3/"

callProxy :: (HasCallStack, MakesValue caller) => String -> caller -> String -> [(String, String)] -> App Response
callProxy pathPrefix caller path qparams = do
  req <- baseRequest caller WireProxy Unversioned ("/proxy/" <> pathPrefix <> path)
  submit "GET" (req & addQueryParams qparams)
