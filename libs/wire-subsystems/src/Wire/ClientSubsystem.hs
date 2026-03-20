{-# LANGUAGE TemplateHaskell #-}

module Wire.ClientSubsystem where

import Data.Id
import Data.Qualified
import Data.Time.Clock
import Imports
import Polysemy
import Wire.API.User.Client
import Wire.API.UserMap

data ClientSubsystem m a where
  InternalGetActivityTimestamps :: UserId -> ClientSubsystem m [Maybe UTCTime]
  LookupLocalClient :: UserId -> ClientId -> ClientSubsystem m (Maybe Client)
  LookupLocalClients :: UserId -> ClientSubsystem m [Client]
  LookupLocalPublicClientsBulk :: [UserId] -> ClientSubsystem m (UserMap (Set PubClient))
  LookupPublicClient :: Qualified UserId -> ClientId -> ClientSubsystem m (Maybe PubClient)
  LookupPublicClients :: Qualified UserId -> ClientSubsystem m [PubClient]
  LookupPublicClientsBulk :: [Qualified UserId] -> ClientSubsystem m (QualifiedUserMap (Set PubClient))

makeSem ''ClientSubsystem
