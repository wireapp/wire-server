{-# LANGUAGE RecordWildCards #-}

module Brig.IO.Serialisation where

import Brig.Types.User.Event
import Data.Aeson hiding (json)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Id
import Data.Json.Util
import Data.Qualified
import Imports
import Wire.API.Properties
import Wire.API.User

toPushFormat :: Event -> Object
toPushFormat (UserEvent (UserCreated u)) =
  KeyMap.fromList
    [ "type" .= ("user.new" :: Text),
      "user" .= SelfProfile (u {userIdentity = Nothing})
    ]
toPushFormat (UserEvent (UserActivated u)) =
  KeyMap.fromList
    [ "type" .= ("user.activate" :: Text),
      "user" .= SelfProfile u
    ]
toPushFormat (UserEvent (UserUpdated (UserUpdatedData i n pic acc ass hdl loc mb ssoId ssoIdDel prots))) =
  KeyMap.fromList
    [ "type" .= ("user.update" :: Text),
      "user"
        .= object
          ( "id" .= i
              # "name" .= n
              # "picture" .= pic -- DEPRECATED
              # "accent_id" .= acc
              # "assets" .= ass
              # "handle" .= hdl
              # "locale" .= loc
              # "managed_by" .= mb
              # "sso_id" .= ssoId
              # "sso_id_deleted" .= ssoIdDel
              # "supported_protocols" .= prots
              # []
          )
    ]
toPushFormat (UserEvent (UserIdentityUpdated UserIdentityUpdatedData {..})) =
  KeyMap.fromList
    [ "type" .= ("user.update" :: Text),
      "user"
        .= object
          ( "id" .= eiuId
              # "email" .= eiuEmail
              # "phone" .= eiuPhone
              # []
          )
    ]
toPushFormat (UserEvent (UserIdentityRemoved (UserIdentityRemovedData i e p))) =
  KeyMap.fromList
    [ "type" .= ("user.identity-remove" :: Text),
      "user"
        .= object
          ( "id" .= i
              # "email" .= e
              # "phone" .= p
              # []
          )
    ]
toPushFormat (ConnectionEvent (ConnectionUpdated uc _ name)) =
  KeyMap.fromList $
    "type" .= ("user.connection" :: Text)
      # "connection" .= uc
      # "user" .= case name of
        Just n -> Just $ object ["name" .= n]
        Nothing -> Nothing
      # []
toPushFormat (UserEvent (UserSuspended i)) =
  KeyMap.fromList
    [ "type" .= ("user.suspend" :: Text),
      "id" .= i
    ]
toPushFormat (UserEvent (UserResumed i)) =
  KeyMap.fromList
    [ "type" .= ("user.resume" :: Text),
      "id" .= i
    ]
toPushFormat (UserEvent (UserDeleted qid)) =
  KeyMap.fromList
    [ "type" .= ("user.delete" :: Text),
      "id" .= qUnqualified qid,
      "qualified_id" .= qid
    ]
toPushFormat (UserEvent (UserLegalHoldDisabled i)) =
  KeyMap.fromList
    [ "type" .= ("user.legalhold-disable" :: Text),
      "id" .= i
    ]
toPushFormat (UserEvent (UserLegalHoldEnabled i)) =
  KeyMap.fromList
    [ "type" .= ("user.legalhold-enable" :: Text),
      "id" .= i
    ]
toPushFormat (PropertyEvent (PropertySet _ k v)) =
  KeyMap.fromList
    [ "type" .= ("user.properties-set" :: Text),
      "key" .= k,
      "value" .= propertyValue v
    ]
toPushFormat (PropertyEvent (PropertyDeleted _ k)) =
  KeyMap.fromList
    [ "type" .= ("user.properties-delete" :: Text),
      "key" .= k
    ]
toPushFormat (PropertyEvent (PropertiesCleared _)) =
  KeyMap.fromList
    [ "type" .= ("user.properties-clear" :: Text)
    ]
toPushFormat (ClientEvent (ClientAdded _ c)) =
  KeyMap.fromList
    [ "type" .= ("user.client-add" :: Text),
      "client" .= c
    ]
toPushFormat (ClientEvent (ClientRemoved _ clientId)) =
  KeyMap.fromList
    [ "type" .= ("user.client-remove" :: Text),
      "client" .= IdObject clientId
    ]
toPushFormat (UserEvent (LegalHoldClientRequested payload)) =
  let LegalHoldClientRequestedData targetUser lastPrekey' clientId = payload
   in KeyMap.fromList
        [ "type" .= ("user.legalhold-request" :: Text),
          "id" .= targetUser,
          "last_prekey" .= lastPrekey',
          "client" .= IdObject clientId
        ]
