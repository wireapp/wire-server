{-# LANGUAGE TemplateHaskell #-}

module Wire.UserSubsystem where

import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.User

data UserSubsystem m a where
  GetUserProfile :: Local UserId -> Qualified UserId -> UserSubsystem m (Maybe UserProfile)
  GetUserProfiles :: Local UserId -> [Qualified UserId] -> UserSubsystem m [UserProfile]

makeSem ''UserSubsystem
