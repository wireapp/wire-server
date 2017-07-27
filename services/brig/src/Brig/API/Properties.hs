module Brig.API.Properties
    ( PropertiesDataError (..)
    , setProperty
    , deleteProperty
    , clearProperties
    , Data.lookupProperty
    , Data.lookupPropertyKeys
    ) where

import Brig.App
import Brig.Data.Properties (PropertiesDataError)
import Brig.Types
import Brig.User.Event
import Control.Error
import Control.Monad.Trans.Class
import Data.Id

import qualified Brig.Data.Properties as Data
import qualified Brig.IO.Intra        as Intra

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
