{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module Gundeck.Push.Native.Types
    ( Result  (..)
    , Failure (..)
    , Message (..)
    , Address (Address)
    , addrUser
    , addrTransport
    , addrApp
    , addrToken
    , addrEndpoint
    , addrConn
    , addrClient
    , addrKeys
    , addrFallback
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
import Data.Singletons.TypeLits (Symbol)
import Gundeck.Aws.Arn
import Gundeck.Types

-- | Native push address information of a device.
data Address (s :: Symbol) = Address
    { _addrUser      :: !UserId
    , _addrTransport :: !Transport
    , _addrApp       :: !AppName
    , _addrToken     :: !Token
    , _addrEndpoint  :: !EndpointArn
    , _addrConn      :: !ConnId
    , _addrClient    :: !ClientId
    , _addrKeys      :: !(Maybe SignalingKeys)
    , _addrFallback  :: !(Maybe Transport)
    }

makeLenses ''Address

addrEqualClient :: Address s -> Address s -> Bool
addrEqualClient a a' = _addrConn   a == _addrConn   a'
                    || _addrClient a == _addrClient a'

instance Show (Address s) where
    show a = showString "Address"
           . showString "{ user = " . shows (a^.addrUser)
           . showString ", transport = " . shows (a^.addrTransport)
           . showString ", app = " . shows (a^.addrApp)
           . showString ", endpoint = " . shows (a^.addrEndpoint)
           . showString ", conn = " . shows (a^.addrConn)
           . showString ", client = " . shows (a^.addrClient)
           . showString ", fallback = " . shows (a^.addrFallback)
           $ "}"

data Result s
    = Success !(Address s)
    | Failure !Failure !(Address s)

data Failure
    = PayloadTooLarge
    | EndpointInvalid
    | EndpointDisabled
    | MissingKeys
    | PushException !SomeException
    deriving (Show)

-- | A native push notification (which does not carry the 'Notification', the client has to pull
-- that from the queue).  This is always transient.
--
-- REFACTOR: rename to @data NativePushNotif ... = NativePushNotif { ... }@.
data Message (s :: Symbol) = Notice
    { msgNotificationId :: NotificationId
    , msgPriority       :: Priority
    , msgApsData        :: Maybe ApsData
    }
