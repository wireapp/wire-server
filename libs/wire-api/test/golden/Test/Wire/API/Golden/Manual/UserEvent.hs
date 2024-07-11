-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Manual.UserEvent
  ( testObject_UserEvent_1,
    testObject_UserEvent_2,
    testObject_UserEvent_3,
    testObject_UserEvent_4,
    testObject_UserEvent_5,
    testObject_UserEvent_6,
    testObject_UserEvent_7,
    testObject_UserEvent_8,
    testObject_UserEvent_9,
    testObject_UserEvent_10,
    testObject_UserEvent_11,
    testObject_UserEvent_12,
    testObject_UserEvent_13,
    testObject_UserEvent_14,
    testObject_UserEvent_15,
    testObject_UserEvent_16,
    testObject_UserEvent_17,
  )
where

import Data.Aeson (toJSON)
import Data.Domain
import Data.ISO3166_CountryCodes
import Data.Id
import Data.Json.Util
import Data.LanguageCodes as L
import Data.Qualified
import Data.UUID qualified as UUID (fromString)
import Imports
import Wire.API.Connection
import Wire.API.Properties
import Wire.API.User
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.UserEvent

testObject_UserEvent_1 :: Event
testObject_UserEvent_1 = UserEvent (UserCreated alice)

testObject_UserEvent_2 :: Event
testObject_UserEvent_2 = UserEvent (UserActivated alice)

testObject_UserEvent_3 :: Event
testObject_UserEvent_3 =
  UserEvent
    ( UserSuspended
        (Id (fromJust (UUID.fromString "dd56271c-181a-43f5-874b-1a8951f7fcc7")))
    )

testObject_UserEvent_4 :: Event
testObject_UserEvent_4 =
  UserEvent
    ( UserSuspended
        (Id (fromJust (UUID.fromString "3ddb960e-8ea3-4d14-95bc-97f9da795ca6")))
    )

testObject_UserEvent_5 :: Event
testObject_UserEvent_5 =
  UserEvent
    ( UserDeleted
        ( Qualified
            (Id (fromJust (UUID.fromString "78f9ba2e-a6b0-48c6-a644-662617bb8bcc")))
            (Domain "bar.example.com")
        )
    )

testObject_UserEvent_6 :: Event
testObject_UserEvent_6 =
  UserEvent
    ( UserUpdated
        ( UserUpdatedData
            (userId alice)
            (Just alice.userDisplayName)
            alice.userTextStatus
            (Just alice.userPict)
            (Just alice.userAccentId)
            (Just alice.userAssets)
            alice.userHandle
            (Just alice.userLocale)
            (Just alice.userManagedBy)
            Nothing
            False
            (Just mempty)
        )
    )

testObject_UserEvent_7 :: Event
testObject_UserEvent_7 =
  UserEvent
    ( UserIdentityUpdated
        ( UserIdentityUpdatedData
            (userId alice)
            (Just (Email "alice" "foo.example.com"))
            Nothing
        )
    )

testObject_UserEvent_8 :: Event
testObject_UserEvent_8 =
  UserEvent
    ( UserIdentityRemoved
        ( UserIdentityRemovedData
            (userId alice)
            (Just (Email "alice" "foo.example.com"))
            Nothing
        )
    )

testObject_UserEvent_9 :: Event
testObject_UserEvent_9 = UserEvent (UserLegalHoldDisabled (userId alice))

testObject_UserEvent_10 :: Event
testObject_UserEvent_10 = UserEvent (UserLegalHoldEnabled (userId alice))

testObject_UserEvent_11 :: Event
testObject_UserEvent_11 =
  UserEvent
    ( LegalHoldClientRequested
        ( LegalHoldClientRequestedData
            (userId alice)
            (lastPrekey "foo")
            (ClientId 3728)
        )
    )

testObject_UserEvent_12 :: Event
testObject_UserEvent_12 =
  ConnectionEvent
    ( ConnectionUpdated
        ( UserConnection
            (userId bob)
            bob.userQualifiedId
            Accepted
            (fromJust (readUTCTimeMillis "2007-02-03T10:51:17.329Z"))
            Nothing
        )
        (Just (Name "hi bob"))
    )

testObject_UserEvent_13 :: Event
testObject_UserEvent_13 =
  PropertyEvent
    ( PropertySet (PropertyKey "a") (toJSON (39 :: Int))
    )

testObject_UserEvent_14 :: Event
testObject_UserEvent_14 =
  PropertyEvent
    ( PropertyDeleted (PropertyKey "a")
    )

testObject_UserEvent_15 :: Event
testObject_UserEvent_15 = PropertyEvent PropertiesCleared

testObject_UserEvent_16 :: Event
testObject_UserEvent_16 =
  ClientEvent
    ( ClientAdded
        ( Client
            (ClientId 2839)
            PermanentClientType
            (fromJust (readUTCTimeMillis "2007-02-03T10:51:17.329Z"))
            (Just DesktopClient)
            (Just "%*")
            Nothing
            (Just "bazz")
            (ClientCapabilityList mempty)
            mempty
            Nothing
        )
    )

testObject_UserEvent_17 :: Event
testObject_UserEvent_17 = ClientEvent (ClientRemoved (ClientId 2839))

--------------------------------------------------------------------------------

alice :: User
alice =
  User
    { userQualifiedId =
        Qualified
          { qUnqualified = Id (fromJust (UUID.fromString "539d9183-32a5-4fc4-ba5c-4634454e7585")),
            qDomain = Domain {_domainText = "foo.example.com"}
          },
      userIdentity = Nothing,
      userDisplayName = Name "alice",
      userTextStatus = Nothing,
      userPict = Pict {fromPict = []},
      userAssets = [],
      userAccentId = ColourId {fromColourId = 1},
      userDeleted = True,
      userLocale =
        Locale
          { lLanguage = Language L.TN,
            lCountry = Just (Country {fromCountry = SB})
          },
      userService = Nothing,
      userHandle = Nothing,
      userExpire = Nothing,
      userTeam = Nothing,
      userManagedBy = ManagedByWire,
      userSupportedProtocols = defSupportedProtocols
    }

bob :: User
bob =
  User
    { userQualifiedId =
        Qualified
          { qUnqualified = Id (fromJust (UUID.fromString "284d1c86-5117-4c58-aa18-c0068f3f7d8c")),
            qDomain = Domain {_domainText = "baz.example.com"}
          },
      userIdentity = Nothing,
      userDisplayName = Name "bob",
      userTextStatus = Nothing,
      userPict = Pict {fromPict = []},
      userAssets = [],
      userAccentId = ColourId {fromColourId = 2},
      userDeleted = False,
      userLocale =
        Locale
          { lLanguage = Language L.CA,
            lCountry = Just (Country {fromCountry = JP})
          },
      userService = Nothing,
      userHandle = Nothing,
      userExpire = Nothing,
      userTeam = Nothing,
      userManagedBy = ManagedByWire,
      userSupportedProtocols = defSupportedProtocols
    }
