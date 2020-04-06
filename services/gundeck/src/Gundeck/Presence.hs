-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
