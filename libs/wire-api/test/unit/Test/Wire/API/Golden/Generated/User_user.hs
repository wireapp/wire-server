{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
import Data.Handle (Handle (Handle, fromHandle))
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
import qualified Data.LanguageCodes
  ( ISO639_1
      ( BI,
        DA,
        TG,
        TN
      ),
  )
import Data.Qualified (Qualified (Qualified, qDomain, qUnqualified))
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust)
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))
import Wire.API.User
  ( Asset (ImageAsset),
    AssetSize (AssetComplete),
    ColourId (ColourId, fromColourId),
    Country (Country, fromCountry),
    Email (Email, emailDomain, emailLocal),
    Language (Language),
    Locale (Locale, lCountry, lLanguage),
    ManagedBy (ManagedByScim, ManagedByWire),
    Name (Name, fromName),
    Phone (Phone, fromPhone),
    Pict (Pict, fromPict),
    User (..),
    UserIdentity
      ( EmailIdentity,
        FullIdentity,
        PhoneIdentity,
        SSOIdentity
      ),
    UserSSOId (UserScimExternalId),
  )

testObject_User_user_1 :: User
testObject_User_user_1 =
  User
    { userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
      userQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))),
            qDomain = Domain {_domainText = "s-f4.s"}
          },
      userIdentity = Nothing,
      userDisplayName = Name {fromName = "\NULuv\996028su\28209lRi"},
      userPict = Pict {fromPict = []},
      userAssets = [],
      userAccentId = ColourId {fromColourId = 1},
      userDeleted = True,
      userLocale = Locale {lLanguage = Language Data.LanguageCodes.TN, lCountry = Just (Country {fromCountry = SB})},
      userService = Nothing,
      userHandle = Nothing,
      userExpire = Nothing,
      userTeam = Nothing,
      userManagedBy = ManagedByWire
    }

testObject_User_user_2 :: User
testObject_User_user_2 =
  User
    { userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
      userQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))),
            qDomain = Domain {_domainText = "k.vbg.p"}
          },
      userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+837934954"})),
      userDisplayName =
        Name
          { fromName =
              "4\1067195\&7\ACK\DC2\DC2\ETBbp\SOH\40601\&0Yr\\\984611vKRg\1048403)\1040186S\983500\1057766:3B\ACK\DC3\ETXT"
          },
      userPict = Pict {fromPict = []},
      userAssets =
        [ (ImageAsset "" (Nothing)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Just AssetComplete))
        ],
      userAccentId = ColourId {fromColourId = -2},
      userDeleted = True,
      userLocale = Locale {lLanguage = Language Data.LanguageCodes.DA, lCountry = Just (Country {fromCountry = TN})},
      userService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
              }
          ),
      userHandle = Nothing,
      userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T17:06:58.936Z")),
      userTeam = Nothing,
      userManagedBy = ManagedByWire
    }

testObject_User_user_3 :: User
testObject_User_user_3 =
  User
    { userId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))),
      userQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))),
            qDomain = Domain {_domainText = "dt.n"}
          },
      userIdentity = Just (EmailIdentity (Email {emailLocal = "f", emailDomain = "\83115"})),
      userDisplayName =
        Name {fromName = ",r\EMXEg0$\98187\RS\SI'uS\ETX/\1009222`\228V.J{\fgE(\rK!\SOp8s9gXO\21810Xj\STX\RS\DC2"},
      userPict = Pict {fromPict = []},
      userAssets = [],
      userAccentId = ColourId {fromColourId = -2},
      userDeleted = True,
      userLocale = Locale {lLanguage = Language Data.LanguageCodes.TG, lCountry = Just (Country {fromCountry = UA})},
      userService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
              }
          ),
      userHandle = Just (Handle {fromHandle = "1c"}),
      userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T20:12:05.821Z")),
      userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))),
      userManagedBy = ManagedByWire
    }

testObject_User_user_4 :: User
testObject_User_user_4 =
  User
    { userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))),
      userQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))),
            qDomain = Domain {_domainText = "28b.cqb"}
          },
      userIdentity =
        Just (SSOIdentity (UserScimExternalId "") (Just (Email {emailLocal = "", emailDomain = ""})) Nothing),
      userDisplayName =
        Name
          { fromName =
              "^\1025896F\1083260=&o>f<7\SOq|6\DC1\EM\997351\1054148\ESCf\1014774\170183\DC3bnVAj`^L\f\1047425\USLI\ENQ!\1061384\ETB`\1041537\ETXe\26313\SUBK|"
          },
      userPict = Pict {fromPict = []},
      userAssets = [],
      userAccentId = ColourId {fromColourId = 0},
      userDeleted = False,
      userLocale = Locale {lLanguage = Language Data.LanguageCodes.BI, lCountry = Just (Country {fromCountry = MQ})},
      userService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
              }
          ),
      userHandle =
        Just
          ( Handle
              { fromHandle =
                  "iw2-.udd2l7-7yg3dfg.wzn4vx3hjhch8.--5t6uyjqk93twv-a2pce8p1xjh7387nztzu.q"
              }
          ),
      userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T14:25:26.089Z")),
      userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))),
      userManagedBy = ManagedByScim
    }

testObject_User_user_5 :: User
testObject_User_user_5 =
  User
    { userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))),
      userQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))),
            qDomain = Domain {_domainText = "28b.cqb"}
          },
      userIdentity =
        Just (FullIdentity (Email {emailLocal = "", emailDomain = ""}) (Phone {fromPhone = "+837934954"})),
      userDisplayName =
        Name
          { fromName =
              "^\1025896F\1083260=&o>f<7\SOq|6\DC1\EM\997351\1054148\ESCf\1014774\170183\DC3bnVAj`^L\f\1047425\USLI\ENQ!\1061384\ETB`\1041537\ETXe\26313\SUBK|"
          },
      userPict = Pict {fromPict = []},
      userAssets = [],
      userAccentId = ColourId {fromColourId = 0},
      userDeleted = False,
      userLocale = Locale {lLanguage = Language Data.LanguageCodes.BI, lCountry = Just (Country {fromCountry = MQ})},
      userService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
              }
          ),
      userHandle =
        Just
          ( Handle
              { fromHandle =
                  "iw2-.udd2l7-7yg3dfg.wzn4vx3hjhch8.--5t6uyjqk93twv-a2pce8p1xjh7387nztzu.q"
              }
          ),
      userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T14:25:26.089Z")),
      userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))),
      userManagedBy = ManagedByScim
    }
