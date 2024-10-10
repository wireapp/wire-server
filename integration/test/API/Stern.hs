module API.Stern where

import API.BrigCommon
import API.Common
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import Data.Function
import Data.Maybe
import Testlib.Prelude

getTeamActivity :: (HasCallStack, MakesValue domain) => domain -> String -> App Response
getTeamActivity domain tid =
  baseRequest domain Stern Unversioned (joinHttpPath ["team-activity-info", tid])
    >>= submit "GET"
