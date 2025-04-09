module API.Proxy where

import Data.String.Conversions
import Network.HTTP.Client (RequestBody (RequestBodyBS))
import Testlib.Prelude

getGiphy :: (HasCallStack, MakesValue caller) => caller -> String -> [(String, String)] -> App Response
getGiphy = callGetProxy "giphy/v1/gifs/"

getYoutube :: (HasCallStack, MakesValue caller) => caller -> String -> [(String, String)] -> App Response
getYoutube = callGetProxy "youtube/v3/"

getGoogleMaps :: (HasCallStack, MakesValue caller) => caller -> String -> [(String, String)] -> App Response
getGoogleMaps = callGetProxy "googlemaps/"

getSoundcloud :: (HasCallStack, MakesValue caller) => caller -> String -> [(String, String)] -> App Response
getSoundcloud = callGetProxy "soundcloud/"

postSpotify :: (HasCallStack, MakesValue caller) => caller -> String -> String -> App Response
postSpotify = callPostProxy "spotify/"

callPostProxy :: (HasCallStack, MakesValue caller) => String -> caller -> String -> String -> App Response
callPostProxy pathPrefix caller path body = do
  req <- baseRequest caller WireProxy Unversioned ("/proxy/" <> pathPrefix <> path)
  submit "POST" (req & addBody (RequestBodyBS (cs body)) "application/json")

callGetProxy :: (HasCallStack, MakesValue caller) => String -> caller -> String -> [(String, String)] -> App Response
callGetProxy pathPrefix caller path qparams = do
  req <- baseRequest caller WireProxy Unversioned ("/proxy/" <> pathPrefix <> path)
  submit "GET" (req & addQueryParams qparams)
