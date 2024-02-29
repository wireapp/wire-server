{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Types.User.Event where

import Control.Lens.TH
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Conversion
import Data.Handle (Handle)
import Data.Id
import Data.Json.Util
import Data.Qualified
import Data.Schema
import Imports
import System.Logger.Message hiding (field, (.=))
import Wire.API.Connection
import Wire.API.Properties
import Wire.API.User
import Wire.API.User.Client
import Wire.API.User.Client.Prekey

data Event
  = UserEvent !UserEvent
  | ConnectionEvent !ConnectionEvent
  | PropertyEvent !PropertyEvent
  | ClientEvent !ClientEvent

eventType :: Event -> EventType
eventType (UserEvent (UserCreated _)) = EventTypeUserCreated
eventType (UserEvent (UserActivated _)) = EventTypeUserActivated
eventType (UserEvent (UserSuspended _)) = EventTypeUserSuspended
eventType (UserEvent (UserResumed _)) = EventTypeUserResumed
eventType (UserEvent (UserDeleted _)) = EventTypeUserDeleted
eventType (UserEvent (UserUpdated _)) = EventTypeUserUpdated
eventType (UserEvent (UserIdentityUpdated _)) = EventTypeUserUpdated
eventType (UserEvent (UserIdentityRemoved _)) = EventTypeUserIdentityRemoved
eventType (UserEvent (UserLegalHoldDisabled _)) = EventTypeUserLegalholdDisabled
eventType (UserEvent (UserLegalHoldEnabled _)) = EventTypeUserLegalholdEnabled
eventType (UserEvent (LegalHoldClientRequested _)) = EventTypeUserLegalholdRequested
eventType (ConnectionEvent _) = EventTypeConnection
eventType (PropertyEvent (PropertySet _ _ _)) = EventTypePropertiesSet
eventType (PropertyEvent (PropertyDeleted _ _)) = EventTypePropertiesDeleted
eventType (PropertyEvent (PropertiesCleared _)) = EventTypePropertiesCleared
eventType (ClientEvent (ClientAdded _ _)) = EventTypeClientAdded
eventType (ClientEvent (ClientRemoved _ _)) = EventTypeClientRemoved

data EventType
  = EventTypeUserCreated
  | EventTypeUserActivated
  | EventTypeUserUpdated
  | EventTypeUserIdentityRemoved
  | EventTypeUserSuspended
  | EventTypeUserResumed
  | EventTypeUserDeleted
  | EventTypeUserLegalholdEnabled
  | EventTypeUserLegalholdDisabled
  | EventTypeUserLegalholdRequested
  | EventTypePropertiesSet
  | EventTypePropertiesDeleted
  | EventTypePropertiesCleared
  | EventTypeClientAdded
  | EventTypeClientRemoved
  | EventTypeConnection
  deriving (Eq, Enum, Bounded)

instance ToSchema EventType where
  schema =
    enum @Text "EventType" $
      mconcat
        [ element "user.new" EventTypeUserCreated,
          element "user.activate" EventTypeUserActivated,
          element "user.update" EventTypeUserUpdated,
          element "user.identity-remove" EventTypeUserIdentityRemoved,
          element "user.suspend" EventTypeUserSuspended,
          element "user.resume" EventTypeUserResumed,
          element "user.delete" EventTypeUserDeleted,
          element "user.legalhold-enable" EventTypeUserLegalholdEnabled,
          element "user.legalhold-disable" EventTypeUserLegalholdDisabled,
          element "user.legalhold-requested" EventTypeUserLegalholdRequested,
          element "user.properties-set" EventTypePropertiesSet,
          element "user.properties-delete" EventTypePropertiesDeleted,
          element "user.properties-clear" EventTypePropertiesCleared,
          element "user.client-add" EventTypeClientAdded,
          element "user.client-remove" EventTypeClientRemoved,
          element "user.connection" EventTypeConnection
        ]

data UserEvent
  = UserCreated !User
  | -- | A user is activated when the first user identity (email address or phone number)
    -- is verified. {#RefActivationEvent}
    UserActivated !User
  | -- | Account & API access of a user has been suspended.
    UserSuspended !UserId
  | -- | Account & API access of a previously suspended user
    -- has been restored.
    UserResumed !UserId
  | -- | The user account has been deleted.
    UserDeleted !(Qualified UserId)
  | UserUpdated !UserUpdatedData
  | UserIdentityUpdated !UserIdentityUpdatedData
  | UserIdentityRemoved !UserIdentityRemovedData
  | UserLegalHoldDisabled !UserId
  | UserLegalHoldEnabled !UserId
  | LegalHoldClientRequested LegalHoldClientRequestedData

data ConnectionEvent = ConnectionUpdated
  { ucConn :: !UserConnection,
    ucName :: !(Maybe Name)
  }

data PropertyEvent
  = PropertySet !UserId !PropertyKey !A.Value
  | PropertyDeleted !UserId !PropertyKey
  | PropertiesCleared !UserId

data ClientEvent
  = ClientAdded !UserId !Client
  | ClientRemoved !UserId !ClientId

data UserUpdatedData = UserUpdatedData
  { eupId :: !UserId,
    eupName :: !(Maybe Name),
    -- | DEPRECATED
    eupPict :: !(Maybe Pict),
    eupAccentId :: !(Maybe ColourId),
    eupAssets :: !(Maybe [Asset]),
    eupHandle :: !(Maybe Handle),
    eupLocale :: !(Maybe Locale),
    eupManagedBy :: !(Maybe ManagedBy),
    eupSSOId :: !(Maybe UserSSOId),
    eupSSOIdRemoved :: Bool,
    eupSupportedProtocols :: !(Maybe (Set BaseProtocolTag))
  }
  deriving stock (Show)

data UserIdentityUpdatedData = UserIdentityUpdatedData
  { eiuId :: !UserId,
    eiuEmail :: !(Maybe Email),
    eiuPhone :: !(Maybe Phone)
  }
  deriving stock (Show)

data UserIdentityRemovedData = UserIdentityRemovedData
  { eirId :: !UserId,
    eirEmail :: !(Maybe Email),
    eirPhone :: !(Maybe Phone)
  }
  deriving stock (Show)

data LegalHoldClientRequestedData = LegalHoldClientRequestedData
  { -- | the user that is under legalhold
    lhcTargetUser :: !UserId,
    -- | the last prekey of the user that is under legalhold
    lhcLastPrekey :: !LastPrekey,
    -- | the client id of the legalhold device
    lhcClientId :: !ClientId
  }
  deriving stock (Show)

emailRemoved :: UserId -> Email -> UserEvent
emailRemoved u e =
  UserIdentityRemoved $ UserIdentityRemovedData u (Just e) Nothing

phoneRemoved :: UserId -> Phone -> UserEvent
phoneRemoved u p =
  UserIdentityRemoved $ UserIdentityRemovedData u Nothing (Just p)

emailUpdated :: UserId -> Email -> UserEvent
emailUpdated u e =
  UserIdentityUpdated $ UserIdentityUpdatedData u (Just e) Nothing

phoneUpdated :: UserId -> Phone -> UserEvent
phoneUpdated u p =
  UserIdentityUpdated $ UserIdentityUpdatedData u Nothing (Just p)

handleUpdated :: UserId -> Handle -> UserEvent
handleUpdated u h =
  UserUpdated $ (emptyUserUpdatedData u) {eupHandle = Just h}

localeUpdate :: UserId -> Locale -> UserEvent
localeUpdate u loc =
  UserUpdated $ (emptyUserUpdatedData u) {eupLocale = Just loc}

managedByUpdate :: UserId -> ManagedBy -> UserEvent
managedByUpdate u mb =
  UserUpdated $ (emptyUserUpdatedData u) {eupManagedBy = Just mb}

supportedProtocolUpdate :: UserId -> Set BaseProtocolTag -> UserEvent
supportedProtocolUpdate u prots =
  UserUpdated $ (emptyUserUpdatedData u) {eupSupportedProtocols = Just prots}

profileUpdated :: UserId -> UserUpdate -> UserEvent
profileUpdated u UserUpdate {..} =
  UserUpdated $
    (emptyUserUpdatedData u)
      { eupName = uupName,
        eupPict = uupPict,
        eupAccentId = uupAccentId,
        eupAssets = uupAssets
      }

emptyUpdate :: UserId -> UserEvent
emptyUpdate = UserUpdated . emptyUserUpdatedData

emptyUserUpdatedData :: UserId -> UserUpdatedData
emptyUserUpdatedData u =
  UserUpdatedData
    { eupId = u,
      eupName = Nothing,
      eupPict = Nothing,
      eupAccentId = Nothing,
      eupAssets = Nothing,
      eupHandle = Nothing,
      eupLocale = Nothing,
      eupManagedBy = Nothing,
      eupSSOId = Nothing,
      eupSSOIdRemoved = False,
      eupSupportedProtocols = Nothing
    }

-- Event schema

$(makePrisms ''Event)
$(makePrisms ''UserEvent)
$(makePrisms ''PropertyEvent)
$(makePrisms ''ClientEvent)

eventObjectSchema :: ObjectSchema SwaggerDoc Event
eventObjectSchema =
  snd
    <$> bind
      (eventType .= field "type" schema)
      ( dispatch $ \case
          EventTypeUserCreated ->
            tag _UserEvent (tag _UserCreated (noId .= userSchema))
          EventTypeUserActivated ->
            tag _UserEvent (tag _UserActivated userSchema)
          EventTypeUserUpdated ->
            tag
              _UserEvent
              ( tag
                  _UserUpdated
                  ( UserUpdatedData
                      <$> eupId .= field "id" schema
                      <*> eupName .= maybe_ (optField "name" schema)
                      <*> eupPict .= maybe_ (optField "picture" schema) -- DEPRECATED
                      <*> eupAccentId .= maybe_ (optField "accent_id" schema)
                      <*> eupAssets .= maybe_ (optField "assets" (array schema))
                      <*> eupHandle .= maybe_ (optField "handle" schema)
                      <*> eupLocale .= maybe_ (optField "locale" schema)
                      <*> eupManagedBy .= maybe_ (optField "managed_by" schema)
                      <*> eupSSOId .= maybe_ (optField "sso_id" genericToSchema)
                      <*> eupSSOIdRemoved .= field "sso_id_deleted" schema
                      <*> eupSupportedProtocols
                        .= maybe_
                          ( optField
                              "supported_protocols"
                              (set schema)
                          )
                  )
                  <|> tag
                    _UserIdentityUpdated
                    ( UserIdentityUpdatedData
                        <$> eiuId .= field "id" schema
                        <*> eiuEmail .= maybe_ (optField "email" schema)
                        <*> eiuPhone .= maybe_ (optField "phone" schema)
                    )
              )
          EventTypeUserIdentityRemoved ->
            tag
              _UserEvent
              ( tag
                  _UserIdentityRemoved
                  ( UserIdentityRemovedData
                      <$> eirId .= field "id" schema
                      <*> eirEmail .= maybe_ (optField "email" schema)
                      <*> eirPhone .= maybe_ (optField "phone" schema)
                  )
              )
          EventTypeUserSuspended -> tag _UserEvent (tag _UserSuspended (field "id" schema))
          EventTypeUserResumed -> tag _UserEvent (tag _UserResumed (field "id" schema))
          EventTypeUserDeleted -> tag _UserEvent (tag _UserDeleted (field "id" schema))
          EventTypeUserLegalholdEnabled ->
            tag
              _UserEvent
              ( tag _UserLegalHoldEnabled (field "id" schema)
              )
          EventTypeUserLegalholdDisabled ->
            tag
              _UserEvent
              ( tag _UserLegalHoldDisabled (field "id" schema)
              )
          EventTypeUserLegalholdRequested ->
            tag
              _UserEvent
              ( tag
                  _LegalHoldClientRequested
                  ( LegalHoldClientRequestedData
                      <$> lhcTargetUser .= field "id" schema
                      <*> lhcLastPrekey .= field "last_prekey" schema
                      <*> lhcClientId .= field "client" schema
                  )
              )
          EventTypePropertiesSet ->
            tag
              _PropertyEvent
              ( tag
                  _PropertySet
                  ( (,,)
                      <$> (\(x, _, _) -> x) .= field "id" schema
                      <*> (\(_, x, _) -> x) .= field "key" genericToSchema
                      <*> (\(_, _, x) -> x)
                        .= field "value" jsonValue
                  )
              )
          EventTypePropertiesDeleted ->
            tag
              _PropertyEvent
              ( tag
                  _PropertyDeleted
                  ( (,)
                      <$> fst .= field "id" schema
                      <*> snd .= field "key" genericToSchema
                  )
              )
          EventTypePropertiesCleared ->
            tag
              _PropertyEvent
              ( tag
                  _PropertiesCleared
                  (field "id" schema)
              )
          EventTypeClientAdded ->
            tag
              _ClientEvent
              ( tag
                  _ClientAdded
                  ( (,)
                      <$> fst .= field "id" schema
                      <*> snd .= field "client" schema
                  )
              )
          EventTypeClientRemoved ->
            tag
              _ClientEvent
              ( tag
                  _ClientRemoved
                  ( (,)
                      <$> fst .= field "id" schema
                      <*> snd .= field "client" schema
                  )
              )
          EventTypeConnection ->
            tag
              _ConnectionEvent
              ( ConnectionUpdated
                  <$> ucConn .= field "connection" schema
                  <*> ucName .= maybe_ (optField "user" (object "UserName" (field "name" schema)))
              )
      )
  where
    noId :: User -> User
    noId u = u {userIdentity = Nothing}

    userSchema :: ObjectSchema SwaggerDoc User
    userSchema = field "user" schema

instance ToJSONObject Event where
  toJSONObject = KM.fromList . fold . schemaOut eventObjectSchema

instance ToSchema Event where
  schema = object "UserEvent" eventObjectSchema

-- Logging

connEventUserId :: ConnectionEvent -> UserId
connEventUserId ConnectionUpdated {..} = ucFrom ucConn

propEventUserId :: PropertyEvent -> UserId
propEventUserId (PropertySet u _ _) = u
propEventUserId (PropertyDeleted u _) = u
propEventUserId (PropertiesCleared u) = u

instance ToBytes Event where
  bytes (UserEvent e) = bytes e
  bytes (ConnectionEvent e) = bytes e
  bytes (PropertyEvent e) = bytes e
  bytes (ClientEvent e) = bytes e

instance ToBytes UserEvent where
  bytes (UserCreated u) = val "user.new: " +++ toByteString (userId u)
  bytes (UserActivated u) = val "user.activate: " +++ toByteString (userId u)
  bytes (UserUpdated u) = val "user.update: " +++ toByteString (eupId u)
  bytes (UserIdentityUpdated u) = val "user.update: " +++ toByteString (eiuId u)
  bytes (UserIdentityRemoved u) = val "user.identity-remove: " +++ toByteString (eirId u)
  bytes (UserSuspended u) = val "user.suspend: " +++ toByteString u
  bytes (UserResumed u) = val "user.resume: " +++ toByteString u
  bytes (UserDeleted u) = val "user.delete: " +++ toByteString (qUnqualified u) +++ val "@" +++ toByteString (qDomain u)
  bytes (UserLegalHoldDisabled u) = val "user.legalhold-disable: " +++ toByteString u
  bytes (UserLegalHoldEnabled u) = val "user.legalhold-enable: " +++ toByteString u
  bytes (LegalHoldClientRequested payload) = val "user.legalhold-request: " +++ show payload

instance ToBytes ConnectionEvent where
  bytes e@ConnectionUpdated {} = val "user.connection: " +++ toByteString (connEventUserId e)

instance ToBytes PropertyEvent where
  bytes e@PropertySet {} = val "user.properties-set: " +++ toByteString (propEventUserId e)
  bytes e@PropertyDeleted {} = val "user.properties-delete: " +++ toByteString (propEventUserId e)
  bytes e@PropertiesCleared {} = val "user.properties-clear: " +++ toByteString (propEventUserId e)

instance ToBytes ClientEvent where
  bytes (ClientAdded u _) = val "user.client-add: " +++ toByteString u
  bytes (ClientRemoved u _) = val "user.client-remove: " +++ toByteString u
