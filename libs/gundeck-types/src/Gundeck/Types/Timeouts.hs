-- | Not really about types, but it is good to have these values in a place that is shared
-- between gundeck and cannon, so the two services always agree on them.
module Gundeck.Types.Timeouts where

import Data.Timeout

gundeckToCannonReqTimeout :: Timeout
gundeckToCannonReqTimeout = 3000 # MilliSecond

-- | Must be @< gundeckToCannonReqTimeout@.
sendWsMsgTimeout :: Timeout
sendWsMsgTimeout = 2000 # MilliSecond
