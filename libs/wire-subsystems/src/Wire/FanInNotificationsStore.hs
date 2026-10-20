{-# LANGUAGE TemplateHaskell #-}

module Wire.FanInNotificationsStore where

import Data.Aeson
import Data.Id
import Data.List.NonEmpty qualified as NE
import Data.Qualified
import Imports
import Polysemy
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.Message
import Wire.API.Push.V2 (ApsData, Route)

data FanInPush = FanInPush
  { conn :: Maybe ConnId,
    transient :: Bool,
    route :: Route,
    nativePriority :: Maybe Priority,
    origin :: Maybe UserId,
    targets :: [Target], -- replaces the previous 'recipients'
    json :: Object,
    apsData :: Maybe ApsData,
    isCellsEvent :: Bool
  }

data Target
  = TargetUser UserId
  | TargetUserClients (UserId, NE.NonEmpty ClientId)
  | TargetTeam TeamId -- All users of a team
  | TargetEpoch (GroupId, Epoch) -- All users in an epoch
  | TargetConnections (Qualified UserId) -- All users connected to this user

data FanInNotificationsStore m a where
  PushViaFanIn :: FanInPush -> FanInNotificationsStore m ()

makeSem ''FanInNotificationsStore
