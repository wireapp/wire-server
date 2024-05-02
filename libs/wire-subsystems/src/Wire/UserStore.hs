{-# LANGUAGE TemplateHaskell #-}

module Wire.UserStore where

import Data.Id
import Imports
import Polysemy
import Wire.StoredUser

data UserStore m a where
  GetUser :: UserId -> UserStore m (Maybe StoredUser)

makeSem ''UserStore
