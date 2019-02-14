
module Gundeck.Push.Native.Types
    ( Result  (..)
    , Failure (..)
    , NativePush (..)
    , Address (..)
    , addrUser
    , addrTransport
    , addrApp
    , addrToken
    , addrEndpoint
    , addrConn
    , addrClient
    , addrEqualClient

      -- * Re-Exports
    , EndpointArn
    , ArnEnv     (..)
    , Account    (..)
    , EndpointId (..)
    , mkSnsArn
    , mkEndpointTopic
    ) where

import Imports
import Control.Lens (makeLenses, (^.))
import Data.Id (UserId, ConnId, ClientId)
import Gundeck.Aws.Arn
import Gundeck.Types

-- | Native push address information of a device.
--
-- REFACTOR: PushToken is embedded in this type, that should probably become a tree?  especially since EnpointArn is also nested.
data Address = Address
    { _addrUser      :: !UserId
    , _addrTransport :: !Transport
    , _addrApp       :: !AppName
    , _addrToken     :: !Token
    , _addrEndpoint  :: !EndpointArn
    , _addrConn      :: !ConnId
    , _addrClient    :: !ClientId
    }
  deriving (Eq, Ord)

makeLenses ''Address

addrEqualClient :: Address -> Address -> Bool
addrEqualClient a a' = _addrConn   a == _addrConn   a'
                    || _addrClient a == _addrClient a'

instance Show Address where
    show a = showString "Address"
           . showString "{ user = " . shows (a^.addrUser)
           . showString ", transport = " . shows (a^.addrTransport)
           . showString ", app = " . shows (a^.addrApp)
           . showString ", endpoint = " . shows (a^.addrEndpoint)
           . showString ", conn = " . shows (a^.addrConn)
           . showString ", client = " . shows (a^.addrClient)
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
    { npNotificationid :: NotificationId
    , npPriority       :: Priority
    , npApsData        :: Maybe ApsData
    }
