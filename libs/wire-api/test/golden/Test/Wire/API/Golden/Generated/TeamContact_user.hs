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

module Test.Wire.API.Golden.Generated.TeamContact_user where

import Data.Bool (Bool (True))
import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import Data.UUID qualified as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Team.Role (Role (RoleAdmin, RoleExternalPartner, RoleMember, RoleOwner))
import Wire.API.User
import Wire.API.User.Search (Sso (..), TeamContact (..))

teamContactTemplate :: TeamContact
teamContactTemplate =
  TeamContact
    { teamContactUserId = Id (fromJust (UUID.fromString "")),
      teamContactName = "",
      teamContactColorId = Nothing,
      teamContactHandle = Nothing,
      teamContactTeam = Nothing,
      teamContactEmail = Nothing,
      teamContactCreatedAt = Nothing,
      teamContactManagedBy = Nothing,
      teamContactSAMLIdp = Nothing,
      teamContactRole = Nothing,
      teamContactScimExternalId = Nothing,
      teamContactSso = Nothing,
      teamContactEmailUnvalidated = Nothing,
      teamContactUserGroups = [],
      teamContactSearchable = True
    }

testObject_TeamContact_user_1 :: TeamContact
testObject_TeamContact_user_1 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001")),
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-11T12:52:22.086Z")),
      teamContactSAMLIdp = Just "r",
      teamContactRole = Just RoleAdmin,
      teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f"
    }

testObject_TeamContact_user_2 :: TeamContact
testObject_TeamContact_user_2 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002")),
      teamContactName = "\160469\35044",
      teamContactColorId = Just 2,
      teamContactHandle = Just "",
      teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))),
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-08T03:35:20.125Z")),
      teamContactSAMLIdp = Just "N\DC4",
      teamContactRole = Just RoleExternalPartner
    }

testObject_TeamContact_user_3 :: TeamContact
testObject_TeamContact_user_3 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000")),
      teamContactHandle = Just "",
      teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))),
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T04:40:28.583Z")),
      teamContactManagedBy = Just ManagedByScim,
      teamContactSAMLIdp = Just "\"c`",
      teamContactRole = Just RoleMember,
      teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f")
    }

testObject_TeamContact_user_4 :: TeamContact
testObject_TeamContact_user_4 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002")),
      teamContactHandle = Just "U6",
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactEmailUnvalidated = Just (unsafeEmailAddress "some" "example")
    }

testObject_TeamContact_user_5 :: TeamContact
testObject_TeamContact_user_5 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000")),
      teamContactName = "8",
      teamContactColorId = Just (-3),
      teamContactHandle = Just "\RS",
      teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))),
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T19:22:27.168Z")),
      teamContactSAMLIdp = Just "\12641",
      teamContactRole = Just RoleExternalPartner
    }

testObject_TeamContact_user_6 :: TeamContact
testObject_TeamContact_user_6 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
      teamContactName = "z",
      teamContactManagedBy = Just ManagedByWire
    }

testObject_TeamContact_user_7 :: TeamContact
testObject_TeamContact_user_7 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000")),
      teamContactName = "7",
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-06T11:54:20.119Z")),
      teamContactManagedBy = Just ManagedByWire,
      teamContactRole = Just RoleAdmin,
      teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
      teamContactEmailUnvalidated = Just (unsafeEmailAddress "some" "example")
    }

testObject_TeamContact_user_8 :: TeamContact
testObject_TeamContact_user_8 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
      teamContactName = "\1067719Z",
      teamContactColorId = Just (-1),
      teamContactHandle = Just "\bdL",
      teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))),
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-06T04:27:11.179Z")),
      teamContactManagedBy = Just ManagedByScim,
      teamContactSAMLIdp = Just "",
      teamContactRole = Just RoleMember
    }

testObject_TeamContact_user_9 :: TeamContact
testObject_TeamContact_user_9 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
      teamContactName = "h,",
      teamContactColorId = Just 2,
      teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))),
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-11T18:31:16.554Z")),
      teamContactManagedBy = Just ManagedByWire,
      teamContactSAMLIdp = Just "\164542\US",
      teamContactRole = Just RoleAdmin
    }

testObject_TeamContact_user_10 :: TeamContact
testObject_TeamContact_user_10 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001")),
      teamContactName = "or",
      teamContactColorId = Just 2,
      teamContactHandle = Just "",
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-06T05:51:36.680Z")),
      teamContactManagedBy = Just ManagedByScim,
      teamContactSAMLIdp = Just "P-\EM",
      teamContactRole = Just RoleMember,
      teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f")
    }

testObject_TeamContact_user_11 :: TeamContact
testObject_TeamContact_user_11 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002")),
      teamContactName = "\ACK",
      teamContactColorId = Just (-3),
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactManagedBy = Just ManagedByScim,
      teamContactRole = Just RoleExternalPartner
    }

testObject_TeamContact_user_12 :: TeamContact
testObject_TeamContact_user_12 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000")),
      teamContactName = "\10652w",
      teamContactHandle = Just "",
      teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-06T13:09:44.601Z")),
      teamContactSAMLIdp = Just "\SUB:"
    }

testObject_TeamContact_user_13 :: TeamContact
testObject_TeamContact_user_13 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001")),
      teamContactName = "\SUB\983552P",
      teamContactColorId = Just 0,
      teamContactHandle = Just "S",
      teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))),
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactManagedBy = Just ManagedByScim,
      teamContactSAMLIdp = Just "\993657\a",
      teamContactRole = Just RoleMember,
      teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f"
    }

testObject_TeamContact_user_14 :: TeamContact
testObject_TeamContact_user_14 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
      teamContactName = "`+",
      teamContactColorId = Just (-3),
      teamContactHandle = Just "\"\US\DC4",
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-08T20:31:37.388Z")),
      teamContactManagedBy = Just ManagedByScim,
      teamContactRole = Just RoleExternalPartner,
      teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f"
    }

testObject_TeamContact_user_15 :: TeamContact
testObject_TeamContact_user_15 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002")),
      teamContactName = "\54517}O",
      teamContactHandle = Just "J",
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-11T14:15:19.890Z")),
      teamContactSAMLIdp = Just "",
      teamContactRole = Just RoleExternalPartner,
      teamContactEmailUnvalidated = Just (unsafeEmailAddress "some" "example")
    }

testObject_TeamContact_user_16 :: TeamContact
testObject_TeamContact_user_16 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002")),
      teamContactName = "\ACK6J",
      teamContactColorId = Just (-1),
      teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))),
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-08T15:43:05.866Z")),
      teamContactManagedBy = Just ManagedByWire,
      teamContactSAMLIdp = Just "k",
      teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f"),
      teamContactEmailUnvalidated = Just (unsafeEmailAddress "some" "example")
    }

testObject_TeamContact_user_17 :: TeamContact
testObject_TeamContact_user_17 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001")),
      teamContactName = "/MB",
      teamContactColorId = Just (-3),
      teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))),
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-10T20:50:28.410Z")),
      teamContactManagedBy = Just ManagedByWire,
      teamContactSAMLIdp = Just "\138052",
      teamContactRole = Just RoleOwner,
      teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
      teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f")
    }

testObject_TeamContact_user_18 :: TeamContact
testObject_TeamContact_user_18 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002")),
      teamContactName = "[\1078188C",
      teamContactColorId = Just 3,
      teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))),
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactManagedBy = Just ManagedByWire,
      teamContactSAMLIdp = Just "\DC2",
      teamContactRole = Just RoleOwner,
      teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f")
    }

testObject_TeamContact_user_19 :: TeamContact
testObject_TeamContact_user_19 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002")),
      teamContactColorId = Just (-3),
      teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))),
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-10T11:20:36.673Z")),
      teamContactRole = Just RoleExternalPartner,
      teamContactSso = Just (Sso "https://example.com/issuer/123" "0307979d-c742-4421-954a-9ceb1f22e58f")
    }

testObject_TeamContact_user_20 :: TeamContact
testObject_TeamContact_user_20 =
  teamContactTemplate
    { teamContactUserId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
      teamContactColorId = Just (-3),
      teamContactHandle = Just "0\1085403\1021449",
      teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))),
      teamContactEmail = Just (unsafeEmailAddress "some" "example"),
      teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-06T18:23:32.240Z")),
      teamContactManagedBy = Just ManagedByScim,
      teamContactSAMLIdp = Just "",
      teamContactRole = Just RoleOwner,
      teamContactScimExternalId = Just "0307979d-c742-4421-954a-9ceb1f22e58f",
      teamContactEmailUnvalidated = Just (unsafeEmailAddress "some" "example")
    }
