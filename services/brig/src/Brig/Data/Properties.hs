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

module Brig.Data.Properties
  ( PropertiesDataError (..),
    insertProperty,
    deleteProperty,
    clearProperties,
    lookupProperty,
    lookupPropertyKeys,
    lookupPropertyKeysAndValues,
  )
where

import Cassandra
import Control.Error
import Data.Id
import Imports
import Wire.API.Properties

maxProperties :: Int64
maxProperties = 16

data PropertiesDataError
  = TooManyProperties

insertProperty ::
  (MonadClient m) =>
  UserId ->
  PropertyKey ->
  RawPropertyValue ->
  ExceptT PropertiesDataError m ()
insertProperty u k v = do
  n <- lift . fmap (maybe 0 runIdentity) . retry x1 $ query1 propertyCount (params LocalQuorum (Identity u))
  unless (n < maxProperties) $
    throwE TooManyProperties
  lift . retry x5 $ write propertyInsert (params LocalQuorum (u, k, v))

deleteProperty :: (MonadClient m) => UserId -> PropertyKey -> m ()
deleteProperty u k = retry x5 $ write propertyDelete (params LocalQuorum (u, k))

clearProperties :: (MonadClient m) => UserId -> m ()
clearProperties u = retry x5 $ write propertyReset (params LocalQuorum (Identity u))

lookupProperty :: (MonadClient m) => UserId -> PropertyKey -> m (Maybe RawPropertyValue)
lookupProperty u k =
  fmap runIdentity
    <$> retry x1 (query1 propertySelect (params LocalQuorum (u, k)))

lookupPropertyKeys :: (MonadClient m) => UserId -> m [PropertyKey]
lookupPropertyKeys u =
  map runIdentity
    <$> retry x1 (query propertyKeysSelect (params LocalQuorum (Identity u)))

lookupPropertyKeysAndValues :: (MonadClient m) => UserId -> m [(PropertyKey, RawPropertyValue)]
lookupPropertyKeysAndValues u =
  retry x1 (query propertyKeysValuesSelect (params LocalQuorum (Identity u)))

-------------------------------------------------------------------------------
-- Queries

propertyInsert :: PrepQuery W (UserId, PropertyKey, RawPropertyValue) ()
propertyInsert = "INSERT INTO properties (user, key, value) VALUES (?, ?, ?)"

propertyDelete :: PrepQuery W (UserId, PropertyKey) ()
propertyDelete = "DELETE FROM properties where user = ? and key = ?"

propertyReset :: PrepQuery W (Identity UserId) ()
propertyReset = "DELETE FROM properties where user = ?"

propertySelect :: PrepQuery R (UserId, PropertyKey) (Identity RawPropertyValue)
propertySelect = "SELECT value FROM properties where user = ? and key = ?"

propertyKeysSelect :: PrepQuery R (Identity UserId) (Identity PropertyKey)
propertyKeysSelect = "SELECT key FROM properties where user = ?"

propertyKeysValuesSelect :: PrepQuery R (Identity UserId) (PropertyKey, RawPropertyValue)
propertyKeysValuesSelect = "SELECT key, value FROM properties where user = ?"

propertyCount :: PrepQuery R (Identity UserId) (Identity Int64)
propertyCount = "SELECT COUNT(*) FROM properties where user = ?"
