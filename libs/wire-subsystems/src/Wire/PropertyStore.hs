{-# LANGUAGE TemplateHaskell #-}

module Wire.PropertyStore where

import Data.Id
import Imports
import Polysemy
import Wire.API.Properties

data PropertyStore m a where
  InsertProperty :: UserId -> PropertyKey -> RawPropertyValue -> PropertyStore m ()
  LookupProperty :: UserId -> PropertyKey -> PropertyStore m (Maybe RawPropertyValue)
  CountProperties :: UserId -> PropertyStore m Int
  DeleteProperty :: UserId -> PropertyKey -> PropertyStore m ()
  ClearProperties :: UserId -> PropertyStore m ()
  GetPropertyKeys :: UserId -> PropertyStore m [PropertyKey]
  GetAllProperties :: UserId -> PropertyStore m [(PropertyKey, RawPropertyValue)]

makeSem ''PropertyStore
