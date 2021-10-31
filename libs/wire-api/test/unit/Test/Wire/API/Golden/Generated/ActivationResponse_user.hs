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
module Test.Wire.API.Golden.Generated.ActivationResponse_user where

import Imports (Bool (False, True), Maybe (Just, Nothing))
import Wire.API.User
  ( Email (Email, emailDomain, emailLocal),
    Phone (Phone, fromPhone),
    UserIdentity
      ( EmailIdentity,
        FullIdentity,
        PhoneIdentity,
        SSOIdentity
      ),
    UserSSOId (UserSSOId, UserScimExternalId),
  )
import Wire.API.User.Activation (ActivationResponse (..))
import Wire.API.User.Identity (mkSimpleSampleUref)

testObject_ActivationResponse_user_1 :: ActivationResponse
testObject_ActivationResponse_user_1 =
  ActivationResponse
    { activatedIdentity =
        SSOIdentity
          (UserSSOId mkSimpleSampleUref)
          (Just (Email {emailLocal = "\165918\rZ\a\ESC", emailDomain = "p\131777\62344"}))
          Nothing,
      activatedFirst = False
    }

testObject_ActivationResponse_user_2 :: ActivationResponse
testObject_ActivationResponse_user_2 =
  ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+7397347696479"}), activatedFirst = False}

testObject_ActivationResponse_user_3 :: ActivationResponse
testObject_ActivationResponse_user_3 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (Email {emailLocal = "\10031*;'R\EM\SI\1032685\1041167", emailDomain = "Gw:[T8\34437"}),
      activatedFirst = False
    }

testObject_ActivationResponse_user_4 :: ActivationResponse
testObject_ActivationResponse_user_4 =
  ActivationResponse
    { activatedIdentity =
        FullIdentity (Email {emailLocal = "h\nPr3", emailDomain = ""}) (Phone {fromPhone = "+82309287"}),
      activatedFirst = True
    }

testObject_ActivationResponse_user_5 :: ActivationResponse
testObject_ActivationResponse_user_5 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (Email {emailLocal = "7\1042098m\95296\b\1098765", emailDomain = "AJX*s&\173117\988870p"}),
      activatedFirst = False
    }

testObject_ActivationResponse_user_6 :: ActivationResponse
testObject_ActivationResponse_user_6 =
  ActivationResponse
    { activatedIdentity = SSOIdentity (UserScimExternalId "\an|") Nothing Nothing,
      activatedFirst = False
    }

testObject_ActivationResponse_user_7 :: ActivationResponse
testObject_ActivationResponse_user_7 =
  ActivationResponse
    { activatedIdentity = EmailIdentity (Email {emailLocal = "\98670", emailDomain = ""}),
      activatedFirst = True
    }

testObject_ActivationResponse_user_8 :: ActivationResponse
testObject_ActivationResponse_user_8 =
  ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+0023160115015"}), activatedFirst = True}

testObject_ActivationResponse_user_9 :: ActivationResponse
testObject_ActivationResponse_user_9 =
  ActivationResponse
    { activatedIdentity =
        FullIdentity (Email {emailLocal = "\ENQ?", emailDomain = ""}) (Phone {fromPhone = "+208573659013"}),
      activatedFirst = False
    }

testObject_ActivationResponse_user_10 :: ActivationResponse
testObject_ActivationResponse_user_10 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (Email {emailLocal = "\ACK3", emailDomain = "\f\1040847\1071035\EOT\1003280P\DEL"}),
      activatedFirst = False
    }

testObject_ActivationResponse_user_11 :: ActivationResponse
testObject_ActivationResponse_user_11 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (Email {emailLocal = "z\126214m\146009<\1046292\a\DC31+*", emailDomain = "S\SO\125114"}),
      activatedFirst = True
    }

testObject_ActivationResponse_user_12 :: ActivationResponse
testObject_ActivationResponse_user_12 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (Email {emailLocal = "d4p\r:\STXI5\167701\158743\GS\v", emailDomain = "\51121\100929"}),
      activatedFirst = False
    }

testObject_ActivationResponse_user_13 :: ActivationResponse
testObject_ActivationResponse_user_13 =
  ActivationResponse
    { activatedIdentity = SSOIdentity (UserScimExternalId "#") Nothing (Just (Phone {fromPhone = "+6124426658"})),
      activatedFirst = False
    }

testObject_ActivationResponse_user_14 :: ActivationResponse
testObject_ActivationResponse_user_14 =
  ActivationResponse
    { activatedIdentity =
        SSOIdentity
          (UserScimExternalId "\NUL\US\ETBY")
          (Just (Email {emailLocal = "\66022", emailDomain = "\a\1081391"}))
          Nothing,
      activatedFirst = False
    }

testObject_ActivationResponse_user_15 :: ActivationResponse
testObject_ActivationResponse_user_15 =
  ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+594453349310"}), activatedFirst = False}

testObject_ActivationResponse_user_16 :: ActivationResponse
testObject_ActivationResponse_user_16 =
  ActivationResponse
    { activatedIdentity =
        FullIdentity (Email {emailLocal = "r\FS,\"", emailDomain = "%R\n\164677^"}) (Phone {fromPhone = "+144713467"}),
      activatedFirst = False
    }

testObject_ActivationResponse_user_17 :: ActivationResponse
testObject_ActivationResponse_user_17 =
  ActivationResponse
    { activatedIdentity =
        SSOIdentity
          (UserScimExternalId "")
          (Just (Email {emailLocal = "\155143", emailDomain = "+)"}))
          (Just (Phone {fromPhone = "+703448141"})),
      activatedFirst = True
    }

testObject_ActivationResponse_user_18 :: ActivationResponse
testObject_ActivationResponse_user_18 =
  ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+974462685543005"}), activatedFirst = True}

testObject_ActivationResponse_user_19 :: ActivationResponse
testObject_ActivationResponse_user_19 =
  ActivationResponse
    { activatedIdentity = SSOIdentity (UserSSOId mkSimpleSampleUref) (Just (Email {emailLocal = "R", emailDomain = "K"})) Nothing,
      activatedFirst = False
    }

testObject_ActivationResponse_user_20 :: ActivationResponse
testObject_ActivationResponse_user_20 =
  ActivationResponse
    { activatedIdentity =
        FullIdentity (Email {emailLocal = "", emailDomain = "E"}) (Phone {fromPhone = "+73148778831190"}),
      activatedFirst = False
    }
