module Galley.Intra.Spar
  ( deleteTeam,
  )
where

import Bilge
import Data.ByteString.Conversion
import Data.Id
import Galley.App
import Galley.Intra.Util
import Imports
import Network.HTTP.Types.Method

-- | Notify Spar that a team is being deleted.
deleteTeam :: TeamId -> Galley ()
deleteTeam tid = do
  (h, p) <- sparReq
  _ <-
    call "spar" $
      method DELETE . host h . port p
        . paths ["i", "teams", toByteString' tid]
        . expect2xx
  pure ()
