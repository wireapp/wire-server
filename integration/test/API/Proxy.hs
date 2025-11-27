-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module API.Proxy where

import Data.String.Conversions
import Network.HTTP.Client (Request (redirectCount), RequestBody (RequestBodyBS))
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
  -- Otherwise, we would follow HTTP 302 responses...
  let req' = req {redirectCount = 0}
  submit "GET" (req' & addQueryParams qparams)
