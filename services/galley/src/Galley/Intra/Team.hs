module Galley.Intra.Team where

import Bilge
import Bilge.RPC
import Brig.Types.Team
import Data.ByteString.Conversion
import Data.Id
import Galley.App
import Galley.Intra.Util
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

getSize :: TeamId -> Galley TeamSize
getSize tid = do
  (h, p) <- brigReq
  r <-
    call "brig" $
      method GET . host h . port p
        . paths ["/i/teams", toByteString' tid, "size"]
        . expect2xx
  parseResponse (Error status502 "server-error") r
