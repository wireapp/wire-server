{-# LANGUAGE TemplateHaskell #-}

module Wire.PasswordStore where

import Data.Id
import Imports
import Polysemy
import Wire.API.Password

data PasswordStore m a where
  UpsertHashedPassword :: UserId -> Password -> PasswordStore m ()
  LookupHashedPassword :: UserId -> PasswordStore m (Maybe Password)

makeSem ''PasswordStore
