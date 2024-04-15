{-# LANGUAGE TemplateHaskell #-}

module Wire.UserSubsystem where

import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.User

data UserSubsystem m a where
  GetUserProfile :: UserId -> Qualified UserId -> UserSubsystem m (Maybe UserProfile)
  GetUserProfiles :: UserId -> [Qualified UserId] -> UserSubsystem m [UserProfile]

makeSem ''UserSubsystem
