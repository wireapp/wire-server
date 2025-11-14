-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.PropertyStore.Cassandra where

import Cassandra
import Data.Id
import Imports
import Polysemy
import Polysemy.Embed
import Wire.API.Properties
import Wire.PropertyStore

interpretPropertyStoreCassandra :: (Member (Embed IO) r) => ClientState -> InterpreterFor PropertyStore r
interpretPropertyStoreCassandra casClient =
  interpret $
    runEmbedded (runClient @IO casClient) . embed . \case
      InsertProperty u k v -> insertPropertyImpl u k v
      LookupProperty u k -> lookupPropertyImpl u k
      CountProperties u -> countPropertiesImpl u
      DeleteProperty u k -> deletePropertyImpl u k
      ClearProperties u -> clearPropertieImpl u
      GetPropertyKeys u -> lookupPropertyKeyImpl u
      GetAllProperties u -> getAllPropertiesImpl u

insertPropertyImpl ::
  (MonadClient m) =>
  UserId ->
  PropertyKey ->
  RawPropertyValue ->
  m ()
insertPropertyImpl u k v =
  retry x5 $ write propertyInsert (params LocalQuorum (u, k, v))

deletePropertyImpl :: (MonadClient m) => UserId -> PropertyKey -> m ()
deletePropertyImpl u k = retry x5 $ write propertyDelete (params LocalQuorum (u, k))

clearPropertieImpl :: (MonadClient m) => UserId -> m ()
clearPropertieImpl u = retry x5 $ write propertyReset (params LocalQuorum (Identity u))

lookupPropertyImpl :: (MonadClient m) => UserId -> PropertyKey -> m (Maybe RawPropertyValue)
lookupPropertyImpl u k =
  fmap runIdentity
    <$> retry x1 (query1 propertySelect (params LocalQuorum (u, k)))

lookupPropertyKeyImpl :: (MonadClient m) => UserId -> m [PropertyKey]
lookupPropertyKeyImpl u =
  map runIdentity
    <$> retry x1 (query propertyKeysSelect (params LocalQuorum (Identity u)))

countPropertiesImpl :: (MonadClient m) => UserId -> m Int
countPropertiesImpl u = do
  maybe 0 fromIntegral <$> retry x1 (query1 propertyCount (params LocalQuorum (Identity u)))

getAllPropertiesImpl :: (MonadClient m) => UserId -> m [(PropertyKey, RawPropertyValue)]
getAllPropertiesImpl u =
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
