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

import Control.Lens (Lens', (^.), makeLenses, view)
import Data.Id (ClientId, ConnId, UserId)
import Gundeck.Aws.Arn
import Gundeck.Types
import Gundeck.Types.Push.V2 (PushToken)
import Imports

-- | Native push address information of a device.
data Address
  = Address
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

data NativePush
  = NativePush
      { npNotificationid :: NotificationId,
        npPriority :: Priority,
        npApsData :: Maybe ApsData
      }
