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

import Brig.App (AppIO)
import Brig.Data.Instances ()
import Brig.Types.Properties
import Cassandra
import Control.Error
import Data.Id
import Imports

maxProperties :: Int64
maxProperties = 16

data PropertiesDataError
  = TooManyProperties

insertProperty :: UserId -> PropertyKey -> PropertyValue -> ExceptT PropertiesDataError AppIO ()
insertProperty u k v = do
  n <- lift . fmap (maybe 0 runIdentity) . retry x1 $ query1 propertyCount (params Quorum (Identity u))
  unless (n < maxProperties) $
    throwE TooManyProperties
  lift . retry x5 $ write propertyInsert (params Quorum (u, k, v))

deleteProperty :: UserId -> PropertyKey -> AppIO ()
deleteProperty u k = retry x5 $ write propertyDelete (params Quorum (u, k))

clearProperties :: UserId -> AppIO ()
clearProperties u = retry x5 $ write propertyReset (params Quorum (Identity u))

lookupProperty :: UserId -> PropertyKey -> AppIO (Maybe PropertyValue)
lookupProperty u k =
  fmap runIdentity
    <$> retry x1 (query1 propertySelect (params Quorum (u, k)))

lookupPropertyKeys :: UserId -> AppIO [PropertyKey]
lookupPropertyKeys u =
  map runIdentity
    <$> retry x1 (query propertyKeysSelect (params Quorum (Identity u)))

lookupPropertyKeysAndValues :: UserId -> AppIO PropertyKeysAndValues
lookupPropertyKeysAndValues u =
  PropertyKeysAndValues
    <$> retry x1 (query propertyKeysValuesSelect (params Quorum (Identity u)))

-------------------------------------------------------------------------------
-- Queries

propertyInsert :: PrepQuery W (UserId, PropertyKey, PropertyValue) ()
propertyInsert = "INSERT INTO properties (user, key, value) VALUES (?, ?, ?)"

propertyDelete :: PrepQuery W (UserId, PropertyKey) ()
propertyDelete = "DELETE FROM properties where user = ? and key = ?"

propertyReset :: PrepQuery W (Identity UserId) ()
propertyReset = "DELETE FROM properties where user = ?"

propertySelect :: PrepQuery R (UserId, PropertyKey) (Identity PropertyValue)
propertySelect = "SELECT value FROM properties where user = ? and key = ?"

propertyKeysSelect :: PrepQuery R (Identity UserId) (Identity PropertyKey)
propertyKeysSelect = "SELECT key FROM properties where user = ?"

propertyKeysValuesSelect :: PrepQuery R (Identity UserId) (PropertyKey, PropertyValue)
propertyKeysValuesSelect = "SELECT key, value FROM properties where user = ?"

propertyCount :: PrepQuery R (Identity UserId) (Identity Int64)
propertyCount = "SELECT COUNT(*) FROM properties where user = ?"
