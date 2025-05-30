{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.UserGroupSubsystem where

import Data.Id
import Imports
import Polysemy
import Wire.API.UserGroup

data UserGroupSubsystem m a where
  CreateGroup :: UserId -> NewUserGroup -> UserGroupSubsystem m UserGroup
  GetGroup :: UserId -> UserGroupId -> UserGroupSubsystem m (Maybe UserGroup)

makeSem ''UserGroupSubsystem
