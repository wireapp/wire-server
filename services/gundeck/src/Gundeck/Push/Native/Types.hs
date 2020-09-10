-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Gundeck.Push.Native.Types
  ( Result (..),
    Failure (..),
    NativePush (..),
    Address (..),
    addrUser,
    addrTransport,
    addrApp,
    addrToken,
    addrEndpoint,
    addrConn,
    addrClient,
    addrEqualClient,
    addrPushToken,

    -- * Re-Exports
    EndpointArn,
    ArnEnv (..),
    Account (..),
    EndpointId (..),
    mkSnsArn,
    mkEndpointTopic,
  )
where

import Control.Lens (Lens', makeLenses, view, (^.))
import Data.Id (ClientId, ConnId, UserId)
import Gundeck.Aws.Arn
import Gundeck.Types
import Gundeck.Types.Push.V2 (PushToken)
import Imports

-- | Native push address information of a device.
data Address = Address
  { _addrUser :: !UserId,
    _addrEndpoint :: !EndpointArn,
    _addrConn :: !ConnId,
    _addrPushToken :: !PushToken
  }
  deriving (Eq, Ord)

makeLenses ''Address

addrTransport :: Lens' Address Transport
addrTransport = addrPushToken . tokenTransport

addrApp :: Lens' Address AppName
addrApp = addrPushToken . tokenApp

addrToken :: Lens' Address Token
addrToken = addrPushToken . token

addrClient :: Lens' Address ClientId
addrClient = addrPushToken . tokenClient

addrEqualClient :: Address -> Address -> Bool
addrEqualClient a a' =
  view addrConn a == view addrConn a'
    || view addrClient a == view addrClient a'

instance Show Address where
  show a =
    showString "Address"
      . showString "{ user = "
      . shows (a ^. addrUser)
      . showString ", transport = "
      . shows (a ^. addrTransport)
      . showString ", app = "
      . shows (a ^. addrApp)
      . showString ", endpoint = "
      . shows (a ^. addrEndpoint)
      . showString ", conn = "
      . shows (a ^. addrConn)
      . showString ", client = "
      . shows (a ^. addrClient)
      $ "}"

data Result
  = Success !Address
  | Failure !Failure !Address

data Failure
  = PayloadTooLarge
  | EndpointInvalid
  | EndpointDisabled
  | PushException !SomeException
  deriving (Show)

data NativePush = NativePush
  { npNotificationid :: NotificationId,
    npPriority :: Priority,
    npApsData :: Maybe ApsData
  }
