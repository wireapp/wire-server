{-# LANGUAGE TemplateHaskell #-}

module Wire.UserStore where

import Data.Id
import Imports
import Polysemy
import Wire.API.User

data UserStore m a where
  GetUser :: UserId -> UserStore m (Maybe User)

makeSem ''UserStore
