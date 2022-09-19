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
import Brig.Effects.GundeckAccess (GundeckAccess)
import qualified Brig.IO.Intra as Intra
import Brig.Types.User.Event
import Control.Error
import Data.Id
import Imports
import Polysemy
import Polysemy.Async
import Wire.API.Properties

setProperty ::
  Members '[Async, GundeckAccess] r =>
  UserId ->
  ConnId ->
  PropertyKey ->
  PropertyValue ->
  ExceptT PropertiesDataError (AppT r) ()
setProperty u c k v = do
  wrapClientE $ Data.insertProperty u k (propertyRaw v)
  lift . liftSem $ Intra.onPropertyEvent u c (PropertySet u k v)

deleteProperty ::
  Members '[Async, GundeckAccess] r =>
  UserId ->
  ConnId ->
  PropertyKey ->
  AppT r ()
deleteProperty u c k = do
  wrapClient $ Data.deleteProperty u k
  liftSem $ Intra.onPropertyEvent u c (PropertyDeleted u k)

clearProperties ::
  Members '[Async, GundeckAccess] r =>
  UserId ->
  ConnId ->
  AppT r ()
clearProperties u c = do
  wrapClient $ Data.clearProperties u
  liftSem $ Intra.onPropertyEvent u c (PropertiesCleared u)
