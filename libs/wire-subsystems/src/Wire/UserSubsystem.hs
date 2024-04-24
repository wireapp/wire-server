{-# LANGUAGE TemplateHaskell #-}

module Wire.UserSubsystem where

import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Federation.Error
import Wire.API.User

data UserSubsystem m a where
  -- | First arg is for authorization only.
  GetUserProfile :: Local UserId -> Qualified UserId -> UserSubsystem m (Maybe UserProfile)
  GetUserProfiles :: Local UserId -> [Qualified UserId] -> UserSubsystem m [UserProfile]
  -- | Sometimes we don't have any identity of a requesting user, and local profiles are public.
  GetLocalUserProfile :: Local UserId -> UserSubsystem m (Maybe UserProfile)
  GetLocalUserProfiles :: Local [UserId] -> UserSubsystem m [UserProfile]
  -- | These give us partial success and hide concurrency in the interpreter.
  GetUserProfilesWithErrors :: Local UserId -> [Qualified UserId] -> UserSubsystem m ([(Qualified UserId, FederationError)], [UserProfile])

makeSem ''UserSubsystem
