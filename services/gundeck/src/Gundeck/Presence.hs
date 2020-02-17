module Gundeck.Presence
  ( list,
    listAll,
    add,
    remove,
  )
where

import Data.ByteString.Conversion
import Data.Id
import Data.Predicate
import Gundeck.Monad
import qualified Gundeck.Presence.Data as Data
import Gundeck.Types
import Gundeck.Util
import Imports
import Network.HTTP.Types
import Network.Wai (Request, Response)
import Network.Wai.Utilities

list :: UserId ::: JSON -> Gundeck Response
list (uid ::: _) = setStatus status200 . json <$> Data.list uid

listAll :: List UserId ::: JSON -> Gundeck Response
listAll (uids ::: _) =
  setStatus status200 . json . concat
    <$> Data.listAll (fromList uids)

add :: Request ::: JSON -> Gundeck Response
add (req ::: _) = do
  p <- fromJsonBody (JsonRequest req)
  Data.add p
  return $
    ( setStatus status201
        . addHeader hLocation (toByteString' (resource p))
    )
      empty

remove :: UserId ::: ConnId ::: CannonId -> Gundeck Response
remove _ = return (empty & setStatus status204)
