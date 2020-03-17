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

-- PropertySet event to self
setProperty :: E -> UserId -> ConnId -> PropertyKey -> PropertyValue -> ExceptT PropertiesDataError AppIO ()
setProperty E u c k v = do
  Data.insertProperty u k v
  -- PropertySet event to self
  lift $ Intra.onPropertyEvent E u c (PropertySet u k v)

-- PropertyDeleted event to self
deleteProperty :: E -> UserId -> ConnId -> PropertyKey -> AppIO ()
deleteProperty E u c k = do
  Data.deleteProperty u k
  -- PropertyDeleted event to self
  Intra.onPropertyEvent E u c (PropertyDeleted u k)

-- PropertiesCleared event to self
clearProperties :: E -> UserId -> ConnId -> AppIO ()
clearProperties E u c = do
  Data.clearProperties u
  -- PropertiesCleared event to self
  Intra.onPropertyEvent E u c (PropertiesCleared u)
