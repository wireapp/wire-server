-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
  ( listH,
    listAllH,
    addH,
    removeH,
  )
where

import Data.CommaSeparatedList
import Data.Id
import Gundeck.Monad
import Gundeck.Presence.Data qualified as Data
import Gundeck.Types
import Imports
import Servant.API
import Wire.API.CannonId

listH :: UserId -> Gundeck [Presence]
listH = runWithDefaultRedis . Data.list

listAllH :: CommaSeparatedList UserId -> Gundeck [Presence]
listAllH uids = concat <$> runWithDefaultRedis (Data.listAll (fromCommaSeparatedList uids))

addH :: Presence -> Gundeck (Headers '[Header "Location" Gundeck.Types.URI] NoContent)
addH p = do
  Data.add p
  pure (addHeader (resource p) NoContent)

removeH :: UserId -> ConnId -> CannonId -> Gundeck NoContent
removeH _ _ _ = pure NoContent
