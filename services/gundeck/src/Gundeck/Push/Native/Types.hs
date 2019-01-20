{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module Gundeck.Push.Native.Types
    ( Result  (..)
    , Failure (..)
    , Message (..)
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
import Data.Singletons.TypeLits (Symbol)
import Gundeck.Aws.Arn
import Gundeck.Types

-- | Native push address information of a device.
--
-- REFACTOR: the @s@ phantom type can probably go away, too!
-- REFACTOR: PushToken is embedded in this type, that should probably become a tree?  especially since EnpointArn is also nested.
data Address (s :: Symbol) = Address
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
           $ "}"

data Result s
    = Success !(Address s)
    | Failure !Failure !(Address s)

data Failure
    = PayloadTooLarge
    | EndpointInvalid
    | EndpointDisabled
    | PushException !SomeException
    deriving (Show)

-- | REFACTOR: rename to @data NativePush (s :: Symbol) = NativePush { ntvpNotificationId :: ...@
data Message (s :: Symbol) = Notice
    { msgNotificationid :: NotificationId
    , msgPriority       :: Priority
    , msgApsData        :: Maybe ApsData
    }
