module API.Stern where

import Testlib.Prelude

getTeamActivity :: (HasCallStack, MakesValue domain) => domain -> String -> App Response
getTeamActivity domain tid =
  baseRequest domain Stern Unversioned (joinHttpPath ["team-activity-info", tid])
    >>= submit "GET"
