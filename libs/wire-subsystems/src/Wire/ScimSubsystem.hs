{-# LANGUAGE TemplateHaskell #-}

module Wire.ScimSubsystem where

import Data.Id
import Polysemy
import Web.Scim.Class.Group qualified as SCG
import Wire.API.User.Scim (SparTag)

data ScimSubsystem m a where
  ScimCreateUserGroup :: TeamId -> SCG.Group -> ScimSubsystem m (SCG.StoredGroup SparTag)
  ScimGetUserGroup :: TeamId -> UserGroupId -> ScimSubsystem m (SCG.StoredGroup SparTag)

makeSem ''ScimSubsystem
