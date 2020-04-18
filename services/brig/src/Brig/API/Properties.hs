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

module Brig.API.Properties
  ( PropertiesDataError (..),
    setProperty,
    deleteProperty,
    clearProperties,
    Data.lookupProperty,
    Data.lookupPropertyKeys,
    Data.lookupPropertyKeysAndValues,
  )
where

import Brig.App
import Brig.Data.Properties (PropertiesDataError)
import qualified Brig.Data.Properties as Data
import qualified Brig.IO.Intra as Intra
import Brig.Types
import Brig.User.Event
import Control.Error
import Data.Id
import Imports

setProperty :: UserId -> ConnId -> PropertyKey -> PropertyValue -> ExceptT PropertiesDataError AppIO ()
setProperty u c k v = do
  Data.insertProperty u k v
  lift $ Intra.onPropertyEvent u c (PropertySet u k v)

deleteProperty :: UserId -> ConnId -> PropertyKey -> AppIO ()
deleteProperty u c k = do
  Data.deleteProperty u k
  Intra.onPropertyEvent u c (PropertyDeleted u k)

clearProperties :: UserId -> ConnId -> AppIO ()
clearProperties u c = do
  Data.clearProperties u
  Intra.onPropertyEvent u c (PropertiesCleared u)
