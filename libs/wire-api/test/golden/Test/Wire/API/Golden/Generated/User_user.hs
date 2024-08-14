{-# LANGUAGE OverloadedLists #-}

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

module Test.Wire.API.Golden.Generated.User_user where

import Data.Domain (Domain (Domain, _domainText))
import Data.Handle
import Data.ISO3166_CountryCodes
  ( CountryCode
      ( MQ,
        SB,
        TN,
        UA
      ),
  )
import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import Data.LanguageCodes qualified
  ( ISO639_1
      ( BI,
        DA,
        TG,
        TN
      ),
  )
import Data.Qualified (Qualified (Qualified, qDomain, qUnqualified))
import Data.UUID qualified as UUID (fromString)
import Imports
import Wire.API.Asset
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))
import Wire.API.User

testObject_User_user_1 :: User
testObject_User_user_1 =
  User
    { userQualifiedId =
        Qualified
          { qUnqualified = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002")),
            qDomain = Domain {_domainText = "s-f4.s"}
          },
      userIdentity = Nothing,
      userDisplayName = Name {fromName = "\NULuv\996028su\28209lRi"},
      userTextStatus = Nothing,
      userPict = Pict {fromPict = []},
      userAssets = [],
      userAccentId = ColourId {fromColourId = 1},
      userDeleted = True,
      userLocale = Locale {lLanguage = Language Data.LanguageCodes.TN, lCountry = Just (Country {fromCountry = SB})},
      userService = Nothing,
      userHandle = Nothing,
      userExpire = Nothing,
      userTeam = Nothing,
      userManagedBy = ManagedByWire,
      userSupportedProtocols = defSupportedProtocols
    }

testObject_User_user_2 :: User
testObject_User_user_2 =
  User
    { userQualifiedId =
        Qualified
          { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001")),
            qDomain = Domain {_domainText = "k.vbg.p"}
          },
      userIdentity = Just (EmailIdentity (unsafeEmailAddress "foo" "example.com")),
      userDisplayName =
        Name
          { fromName =
              "4\1067195\&7\ACK\DC2\DC2\ETBbp\SOH\40601\&0Yr\\\984611vKRg\1048403)\1040186S\983500\1057766:3B\ACK\DC3\ETXT"
          },
      userTextStatus = rightToMaybe $ mkTextStatus "text status",
      userPict = Pict {fromPict = []},
      userAssets =
        [ ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) Nothing,
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)
        ],
      userAccentId = ColourId {fromColourId = -2},
      userDeleted = True,
      userLocale = Locale {lLanguage = Language Data.LanguageCodes.DA, lCountry = Just (Country {fromCountry = TN})},
      userService =
        Just
          ( ServiceRef
              { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
                _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))
              }
          ),
      userHandle = Nothing,
      userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T17:06:58.936Z")),
      userTeam = Nothing,
      userManagedBy = ManagedByWire,
      userSupportedProtocols = mempty
    }

testObject_User_user_3 :: User
testObject_User_user_3 =
  User
    { userQualifiedId =
        Qualified
          { qUnqualified = Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002")),
            qDomain = Domain {_domainText = "dt.n"}
          },
      userIdentity = Just (EmailIdentity (unsafeEmailAddress "f" "\83115a.o")),
      userDisplayName =
        Name {fromName = ",r\EMXEg0$\98187\RS\SI'uS\ETX/\1009222`\228V.J{\fgE(\rK!\SOp8s9gXO\21810Xj\STX\RS\DC2"},
      userTextStatus = Nothing,
      userPict = Pict {fromPict = []},
      userAssets = [],
      userAccentId = ColourId {fromColourId = -2},
      userDeleted = True,
      userLocale = Locale {lLanguage = Language Data.LanguageCodes.TG, lCountry = Just (Country {fromCountry = UA})},
      userService =
        Just
          ( ServiceRef
              { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
                _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))
              }
          ),
      userHandle = Just (fromJust (parseHandle "1c")),
      userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T20:12:05.821Z")),
      userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))),
      userManagedBy = ManagedByWire,
      userSupportedProtocols = defSupportedProtocols
    }

testObject_User_user_4 :: User
testObject_User_user_4 =
  User
    { userQualifiedId =
        Qualified
          { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002")),
            qDomain = Domain {_domainText = "28b.cqb"}
          },
      userIdentity =
        Just (SSOIdentity (UserScimExternalId "") (Just (unsafeEmailAddress "not" "empty"))),
      userDisplayName =
        Name
          { fromName =
              "^\1025896F\1083260=&o>f<7\SOq|6\DC1\EM\997351\1054148\ESCf\1014774\170183\DC3bnVAj`^L\f\1047425\USLI\ENQ!\1061384\ETB`\1041537\ETXe\26313\SUBK|"
          },
      userTextStatus = Nothing,
      userPict = Pict {fromPict = []},
      userAssets = [],
      userAccentId = ColourId {fromColourId = 0},
      userDeleted = False,
      userLocale = Locale {lLanguage = Language Data.LanguageCodes.BI, lCountry = Just (Country {fromCountry = MQ})},
      userService =
        Just
          ( ServiceRef
              { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
                _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))
              }
          ),
      userHandle =
        Just (fromJust (parseHandle "iw2-.udd2l7-7yg3dfg.wzn4vx3hjhch8.--5t6uyjqk93twv-a2pce8p1xjh7387nztzu.q")),
      userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T14:25:26.089Z")),
      userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))),
      userManagedBy = ManagedByScim,
      userSupportedProtocols = defSupportedProtocols
    }

testObject_User_user_5 :: User
testObject_User_user_5 =
  User
    { userQualifiedId =
        Qualified
          { qUnqualified = Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002")),
            qDomain = Domain {_domainText = "28b.cqb"}
          },
      userIdentity =
        Just (EmailIdentity (unsafeEmailAddress "bar" "example.com")),
      userDisplayName =
        Name
          { fromName =
              "^\1025896F\1083260=&o>f<7\SOq|6\DC1\EM\997351\1054148\ESCf\1014774\170183\DC3bnVAj`^L\f\1047425\USLI\ENQ!\1061384\ETB`\1041537\ETXe\26313\SUBK|"
          },
      userTextStatus = Nothing,
      userPict = Pict {fromPict = []},
      userAssets = [],
      userAccentId = ColourId {fromColourId = 0},
      userDeleted = False,
      userLocale = Locale {lLanguage = Language Data.LanguageCodes.BI, lCountry = Just (Country {fromCountry = MQ})},
      userService =
        Just
          ( ServiceRef
              { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
                _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))
              }
          ),
      userHandle =
        Just (fromJust (parseHandle "iw2-.udd2l7-7yg3dfg.wzn4vx3hjhch8.--5t6uyjqk93twv-a2pce8p1xjh7387nztzu.q")),
      userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T14:25:26.089Z")),
      userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))),
      userManagedBy = ManagedByScim,
      userSupportedProtocols = defSupportedProtocols
    }
