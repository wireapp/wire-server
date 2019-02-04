{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Brig.User.Event.Log where

import Brig.User.Event
import Data.Id
import Data.ByteString.Conversion
import System.Logger.Class

connection :: UserId -> UserId -> Msg -> Msg
connection from to =
       "connection.from" .= toByteString from
    ~~ "connection.to"   .= toByteString to

instance ToBytes Event where
    bytes (UserEvent e)       = bytes e
    bytes (ConnectionEvent e) = bytes e
    bytes (PropertyEvent e)   = bytes e
    bytes (ClientEvent e)     = bytes e

instance ToBytes UserEvent where
    bytes e@UserCreated{}         = val "user.new: " +++ toByteString (userEventUserId e)
    bytes e@UserActivated{}       = val "user.activate: " +++ toByteString (userEventUserId e)
    bytes e@UserUpdated{}         = val "user.update: " +++ toByteString (userEventUserId e)
    bytes e@UserIdentityUpdated{} = val "user.update: " +++ toByteString (userEventUserId e)
    bytes e@UserIdentityRemoved{} = val "user.identity-remove: " +++ toByteString (userEventUserId e)
    bytes e@UserSuspended{}       = val "user.suspend: " +++ toByteString (userEventUserId e)
    bytes e@UserResumed{}         = val "user.resume: " +++ toByteString (userEventUserId e)
    bytes e@UserDeleted{}         = val "user.delete: " +++ toByteString (userEventUserId e)

instance ToBytes ConnectionEvent where
    bytes e@ConnectionUpdated{} = val "user.connection: " +++ toByteString (connEventUserId e)

instance ToBytes PropertyEvent where
    bytes e@PropertySet{}       = val "user.properties-set: " +++ toByteString (propEventUserId e)
    bytes e@PropertyDeleted{}   = val "user.properties-delete: " +++ toByteString (propEventUserId e)
    bytes e@PropertiesCleared{} = val "user.properties-clear: " +++ toByteString (propEventUserId e)

instance ToBytes ClientEvent where
    bytes (ClientAdded u _)   = val "user.client-add: " +++ toByteString u
    bytes (ClientRemoved u _) = val "user.client-remove: " +++ toByteString u
