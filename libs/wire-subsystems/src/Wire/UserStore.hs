{-# LANGUAGE TemplateHaskell #-}

module Wire.UserStore where

import Data.Id
import Imports
import Polysemy
import Wire.API.User
import Wire.StoredUser

data UserStore m a where
  GetUser :: UserId -> UserStore m (Maybe StoredUser)
  UpdateUser :: UserId -> UserUpdate -> UserStore m ()

makeSem ''UserStore
