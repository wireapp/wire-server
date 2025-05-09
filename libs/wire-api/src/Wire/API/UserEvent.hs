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

module Wire.API.UserEvent where

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
import Wire.API.Locale
import Wire.API.Properties
import Wire.API.Routes.Version
import Wire.API.User
import Wire.API.User.Client
import Wire.API.User.Client.Prekey

data Event
  = UserEvent !UserEvent
  | ConnectionEvent !ConnectionEvent
  | PropertyEvent !PropertyEvent
  | ClientEvent !ClientEvent
  | UserGroupEvent !UserGroupEvent
  deriving stock (Eq, Show)

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
eventType (PropertyEvent (PropertySet _ _)) = EventTypePropertiesSet
eventType (PropertyEvent (PropertyDeleted _)) = EventTypePropertiesDeleted
eventType (PropertyEvent PropertiesCleared) = EventTypePropertiesCleared
eventType (ClientEvent (ClientAdded _)) = EventTypeClientAdded
eventType (ClientEvent (ClientRemoved _)) = EventTypeClientRemoved
eventType (UserGroupEvent (UserGroupCreated _)) = EventTypeUserGroupCreated

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
  | EventTypeUserGroupCreated
  deriving stock (Eq, Enum, Bounded)

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
          element "user.legalhold-request" EventTypeUserLegalholdRequested,
          element "user.properties-set" EventTypePropertiesSet,
          element "user.properties-delete" EventTypePropertiesDeleted,
          element "user.properties-clear" EventTypePropertiesCleared,
          element "user.client-add" EventTypeClientAdded,
          element "user.client-remove" EventTypeClientRemoved,
          element "user.connection" EventTypeConnection,
          element "user-group.created" EventTypeConnection
        ]

data UserEvent
  = UserCreated !User
  | -- | A user is activated when the first user identity (email address)
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
  deriving stock (Eq, Show)

data UserGroupEvent
  = UserGroupCreated !UserGroupId
  deriving stock (Eq, Show)

data ConnectionEvent = ConnectionUpdated
  { ucConn :: !UserConnection,
    ucName :: !(Maybe Name)
  }
  deriving stock (Eq, Show)

data PropertyEvent
  = PropertySet !PropertyKey !A.Value
  | PropertyDeleted !PropertyKey
  | PropertiesCleared
  deriving stock (Eq, Show)

data ClientEvent
  = ClientAdded !Client
  | ClientRemoved !ClientId
  deriving stock (Eq, Show)

data UserUpdatedData = UserUpdatedData
  { eupId :: !UserId,
    eupName :: !(Maybe Name),
    eupTextStatus :: !(Maybe TextStatus),
    -- | DEPRECATED
    eupPict :: !(Maybe Pict),
    eupAccentId :: !(Maybe ColourId),
    eupAssets :: !(Maybe [Asset]),
    eupHandle :: !(Maybe Handle),
    eupLocale :: !(Maybe Locale),
    eupManagedBy :: !(Maybe ManagedBy),
    eupSSOId :: !(Maybe UserSSOId),
    eupSSOIdRemoved :: Bool,
    eupSupportedProtocols :: !(Maybe (Set BaseProtocolTag)),
    eupTeam :: !(Maybe TeamId)
  }
  deriving stock (Eq, Show)

data UserIdentityUpdatedData = UserIdentityUpdatedData
  { eiuId :: !UserId,
    eiuEmail :: !(Maybe EmailAddress),
    eiuPhone :: !(Maybe Phone)
  }
  deriving stock (Eq, Show)

data UserIdentityRemovedData = UserIdentityRemovedData
  { eirId :: !UserId,
    eirEmail :: !(Maybe EmailAddress),
    eirPhone :: !(Maybe Phone)
  }
  deriving stock (Eq, Show)

data LegalHoldClientRequestedData = LegalHoldClientRequestedData
  { -- | the user that is under legalhold
    lhcTargetUser :: !UserId,
    -- | the last prekey of the user that is under legalhold
    lhcLastPrekey :: !LastPrekey,
    -- | the client id of the legalhold device
    lhcClientId :: !ClientId
  }
  deriving stock (Eq, Show)

emailRemoved :: UserId -> EmailAddress -> UserEvent
emailRemoved u e =
  UserIdentityRemoved $ UserIdentityRemovedData u (Just e) Nothing

emailUpdated :: UserId -> EmailAddress -> UserEvent
emailUpdated u e =
  UserIdentityUpdated $ UserIdentityUpdatedData u (Just e) Nothing

teamUpdated :: UserId -> TeamId -> UserEvent
teamUpdated u t = UserUpdated (emptyUserUpdatedData u) {eupTeam = Just t}

emptyUserUpdatedData :: UserId -> UserUpdatedData
emptyUserUpdatedData u =
  UserUpdatedData
    { eupId = u,
      eupName = Nothing,
      eupTextStatus = Nothing,
      eupPict = Nothing,
      eupAccentId = Nothing,
      eupAssets = Nothing,
      eupHandle = Nothing,
      eupLocale = Nothing,
      eupManagedBy = Nothing,
      eupSSOId = Nothing,
      eupSSOIdRemoved = False,
      eupSupportedProtocols = Nothing,
      eupTeam = Nothing
    }

-- Event schema

$(makePrisms ''Event)
$(makePrisms ''UserEvent)
$(makePrisms ''PropertyEvent)
$(makePrisms ''ClientEvent)
$(makePrisms ''UserGroupEvent)

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
                  ( field
                      "user"
                      ( object
                          "UserUpdatedData"
                          ( UserUpdatedData
                              <$> eupId .= field "id" schema
                              <*> eupName .= maybe_ (optField "name" schema)
                              <*> eupTextStatus .= maybe_ (optField "text_status" schema)
                              <*> eupPict .= maybe_ (optField "picture" schema) -- DEPRECATED
                              <*> eupAccentId .= maybe_ (optField "accent_id" schema)
                              <*> eupAssets .= maybe_ (optField "assets" (array schema))
                              <*> eupHandle .= maybe_ (optField "handle" schema)
                              <*> eupLocale .= maybe_ (optField "locale" schema)
                              <*> eupManagedBy .= maybe_ (optField "managed_by" schema)
                              <*> eupSSOId .= maybe_ (optField "sso_id" genericToSchema)
                              <*> eupSSOIdRemoved .= field "sso_id_deleted" schema
                              <*> eupSupportedProtocols .= maybe_ (optField "supported_protocols" (set schema))
                              <*> eupTeam .= maybe_ (optField "team" schema)
                          )
                      )
                  )
                  <|> tag
                    _UserIdentityUpdated
                    ( field
                        "user"
                        ( object
                            "UserIdentityUpdatedData"
                            ( UserIdentityUpdatedData
                                <$> eiuId .= field "id" schema
                                <*> eiuEmail .= maybe_ (optField "email" schema)
                                <*> eiuPhone .= maybe_ (optField "phone" schema)
                            )
                        )
                    )
              )
          EventTypeUserIdentityRemoved ->
            tag
              _UserEvent
              ( tag
                  _UserIdentityRemoved
                  ( field
                      "user"
                      ( object
                          "UserIdentityRemovedData"
                          ( UserIdentityRemovedData
                              <$> eirId .= field "id" schema
                              <*> eirEmail .= maybe_ (optField "email" schema)
                              <*> eirPhone .= maybe_ (optField "phone" schema)
                          )
                      )
                  )
              )
          EventTypeUserSuspended -> tag _UserEvent (tag _UserSuspended (field "id" schema))
          EventTypeUserResumed -> tag _UserEvent (tag _UserResumed (field "id" schema))
          EventTypeUserDeleted ->
            tag
              _UserEvent
              ( tag
                  _UserDeleted
                  ( field "qualified_id" schema
                      <* qUnqualified .= field "id" schema
                  )
              )
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
                      <*> lhcClientId .= field "client" (idObjectSchema schema)
                  )
              )
          EventTypePropertiesSet ->
            tag
              _PropertyEvent
              ( tag
                  _PropertySet
                  ( (,)
                      <$> fst .= field "key" genericToSchema
                      <*> snd .= field "value" jsonValue
                  )
              )
          EventTypePropertiesDeleted ->
            tag
              _PropertyEvent
              ( tag
                  _PropertyDeleted
                  (field "key" genericToSchema)
              )
          EventTypePropertiesCleared ->
            tag
              _PropertyEvent
              ( tag
                  _PropertiesCleared
                  (pure ())
              )
          EventTypeClientAdded ->
            tag
              _ClientEvent
              ( tag
                  _ClientAdded
                  (field "client" (clientSchema (Just V6)))
              )
          EventTypeClientRemoved ->
            tag
              _ClientEvent
              ( tag
                  _ClientRemoved
                  (field "client" (idObjectSchema schema))
              )
          EventTypeConnection ->
            tag
              _ConnectionEvent
              ( ConnectionUpdated
                  <$> ucConn .= field "connection" schema
                  <*> ucName .= maybe_ (optField "user" (object "UserName" (field "name" schema)))
              )
          EventTypeUserGroupCreated ->
            tag
              _UserGroupEvent
              ( tag
                  _UserGroupCreated
                  (field "id" (idObjectSchema schema))
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

deriving via (Schema Event) instance A.ToJSON Event

deriving via (Schema Event) instance A.FromJSON Event

-- Logging

connEventUserId :: ConnectionEvent -> UserId
connEventUserId ConnectionUpdated {..} = ucFrom ucConn

instance ToBytes Event where
  bytes (UserEvent e) = bytes e
  bytes (ConnectionEvent e) = bytes e
  bytes (PropertyEvent e) = bytes e
  bytes (ClientEvent e) = bytes e
  bytes (UserGroupEvent e) = bytes e

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
  bytes PropertySet {} = val "user.properties-set"
  bytes PropertyDeleted {} = val "user.properties-delete"
  bytes PropertiesCleared {} = val "user.properties-clear"

instance ToBytes ClientEvent where
  bytes (ClientAdded _) = val "user.client-add"
  bytes (ClientRemoved _) = val "user.client-remove"

instance ToBytes UserGroupEvent where
  bytes (UserGroupCreated u) = val "user-group.created: " +++ toByteString u
