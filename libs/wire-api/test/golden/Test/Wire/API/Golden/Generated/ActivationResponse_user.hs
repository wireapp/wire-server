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

import Data.These
import Imports (Bool (False, True), Maybe (Just, Nothing), undefined)
import Wire.API.User
import Wire.API.User.Activation (ActivationResponse (..))
import Wire.API.User.Scim (ValidScimId (ValidScimId))

testObject_ActivationResponse_user_1 :: ActivationResponse
testObject_ActivationResponse_user_1 =
  ActivationResponse
    { activatedIdentity =
        SSOIdentity
          (UserSSOId (ValidScimId "" (That mkSimpleSampleUref)))
          (Just (unsafeEmailAddress "some" "example")),
      activatedFirst = False
    }

testObject_ActivationResponse_user_2 :: ActivationResponse
testObject_ActivationResponse_user_2 =
  ActivationResponse {activatedIdentity = EmailIdentity (unsafeEmailAddress "some" "example"), activatedFirst = False}

testObject_ActivationResponse_user_3 :: ActivationResponse
testObject_ActivationResponse_user_3 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (unsafeEmailAddress "some" "example"),
      activatedFirst = False
    }

testObject_ActivationResponse_user_4 :: ActivationResponse
testObject_ActivationResponse_user_4 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (unsafeEmailAddress "some" "example"),
      activatedFirst = True
    }

testObject_ActivationResponse_user_5 :: ActivationResponse
testObject_ActivationResponse_user_5 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (unsafeEmailAddress "some" "example"),
      activatedFirst = False
    }

testObject_ActivationResponse_user_6 :: ActivationResponse
testObject_ActivationResponse_user_6 =
  ActivationResponse
    { activatedIdentity = SSOIdentity (UserSSOId (ValidScimId "\an|" undefined)) Nothing,
      activatedFirst = False
    }

testObject_ActivationResponse_user_7 :: ActivationResponse
testObject_ActivationResponse_user_7 =
  ActivationResponse
    { activatedIdentity = EmailIdentity (unsafeEmailAddress "some" "example"),
      activatedFirst = True
    }

testObject_ActivationResponse_user_8 :: ActivationResponse
testObject_ActivationResponse_user_8 =
  ActivationResponse {activatedIdentity = EmailIdentity (unsafeEmailAddress "some" "example"), activatedFirst = True}

testObject_ActivationResponse_user_9 :: ActivationResponse
testObject_ActivationResponse_user_9 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (unsafeEmailAddress "some" "example"),
      activatedFirst = False
    }

testObject_ActivationResponse_user_10 :: ActivationResponse
testObject_ActivationResponse_user_10 =
  ActivationResponse
    { activatedIdentity =
        EmailIdentity (unsafeEmailAddress "some" "example"),
      activatedFirst = False
    }
