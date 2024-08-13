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

module Test.Wire.API.Golden.Generated.ActivationResponse_user where

import Imports (Bool (False, True), Maybe (Just, Nothing))
import Wire.API.User
import Wire.API.User.Activation (ActivationResponse (..))

testObject_ActivationResponse_user_1 :: ActivationResponse
testObject_ActivationResponse_user_1 =
  ActivationResponse
    { activatedIdentity =
        SSOIdentity
          (UserSSOId mkSimpleSampleUref)
          (Just (unsafeEmailAddress "\165918\rZ\a\ESC" "p\131777\62344")),
      activatedFirst = False
    }

testObject_ActivationResponse_user_2 :: ActivationResponse
testObject_ActivationResponse_user_2 =
  ActivationResponse {activatedIdentity = EmailIdentity (unsafeEmailAddress "foo" "example.com"), activatedFirst = False}

testObject_ActivationResponse_user_3 :: ActivationResponse
testObject_ActivationResponse_user_3 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (unsafeEmailAddress "\10031*;'R\EM\SI\1032685\1041167" "Gw:[T8\34437"),
      activatedFirst = False
    }

testObject_ActivationResponse_user_4 :: ActivationResponse
testObject_ActivationResponse_user_4 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (unsafeEmailAddress "h\nPr3" ""),
      activatedFirst = True
    }

testObject_ActivationResponse_user_5 :: ActivationResponse
testObject_ActivationResponse_user_5 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (unsafeEmailAddress "7\1042098m\95296\b\1098765" "AJX*s&\173117\988870p"),
      activatedFirst = False
    }

testObject_ActivationResponse_user_6 :: ActivationResponse
testObject_ActivationResponse_user_6 =
  ActivationResponse
    { activatedIdentity = SSOIdentity (UserScimExternalId "\an|") Nothing,
      activatedFirst = False
    }

testObject_ActivationResponse_user_7 :: ActivationResponse
testObject_ActivationResponse_user_7 =
  ActivationResponse
    { activatedIdentity = EmailIdentity (unsafeEmailAddress "\98670" ""),
      activatedFirst = True
    }

testObject_ActivationResponse_user_8 :: ActivationResponse
testObject_ActivationResponse_user_8 =
  ActivationResponse {activatedIdentity = EmailIdentity (unsafeEmailAddress "bar" "example.com"), activatedFirst = True}

testObject_ActivationResponse_user_9 :: ActivationResponse
testObject_ActivationResponse_user_9 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (unsafeEmailAddress "\ENQ?" ""),
      activatedFirst = False
    }

testObject_ActivationResponse_user_10 :: ActivationResponse
testObject_ActivationResponse_user_10 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (unsafeEmailAddress "\ACK3" "\f\1040847\1071035\EOT\1003280P\DEL"),
      activatedFirst = False
    }
