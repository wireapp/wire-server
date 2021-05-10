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

module Test.Wire.API.Golden.Generated.Contact_user where

import Data.Domain (Domain (Domain, _domainText))
import Data.Id (Id (Id))
import Data.Qualified
  ( Qualified (Qualified, qDomain, qUnqualified),
  )
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.User.Search (Contact (..))

testObject_Contact_user_1 :: Contact
testObject_Contact_user_1 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000300000005"))), qDomain = Domain {_domainText = "j00.8y.yr3isy2m"}}, contactName = "", contactColorId = Just 6, contactHandle = Just "\1089530\NUL|\SO", contactTeam = Nothing}

testObject_Contact_user_2 :: Contact
testObject_Contact_user_2 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000100000007"))), qDomain = Domain {_domainText = "z.l--66-i8g8a9"}}, contactName = "\SYND", contactColorId = Just (-5), contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000400000002")))}

testObject_Contact_user_3 :: Contact
testObject_Contact_user_3 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000700000003"))), qDomain = Domain {_domainText = "h.y-2k71.rh"}}, contactName = "S\1037187D\GS", contactColorId = Just (-4), contactHandle = Just "\175177~\35955c", contactTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000700000008")))}

testObject_Contact_user_4 :: Contact
testObject_Contact_user_4 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000000000004"))), qDomain = Domain {_domainText = "2347.cye2i7.sn.r2z83.d03"}}, contactName = "@=\ETX", contactColorId = Nothing, contactHandle = Just "6", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000500000004")))}

testObject_Contact_user_5 :: Contact
testObject_Contact_user_5 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000005"))), qDomain = Domain {_domainText = "v0u29n3.er"}}, contactName = "5m~\DC4`", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Nothing}

testObject_Contact_user_6 :: Contact
testObject_Contact_user_6 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000400000000"))), qDomain = Domain {_domainText = "6k.p"}}, contactName = "Cst\995547U", contactColorId = Nothing, contactHandle = Just "qI", contactTeam = Just (Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000600000000")))}

testObject_Contact_user_7 :: Contact
testObject_Contact_user_7 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000800000008"))), qDomain = Domain {_domainText = "yr.e1-d"}}, contactName = "\b74\ENQ", contactColorId = Just 5, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000008-0000-0001-0000-000400000008")))}

testObject_Contact_user_8 :: Contact
testObject_Contact_user_8 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000600000008"))), qDomain = Domain {_domainText = "51r9in-k6i5l8-7y6.t205p-gl2"}}, contactName = "w\1050194\993461#\\", contactColorId = Just (-2), contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0007-0000-000500000002")))}

testObject_Contact_user_9 :: Contact
testObject_Contact_user_9 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000600000008"))), qDomain = Domain {_domainText = "37-p6v67.g"}}, contactName = ",\1041199 \v\1077257", contactColorId = Just 5, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000500000000")))}

testObject_Contact_user_10 :: Contact
testObject_Contact_user_10 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000800000007"))), qDomain = Domain {_domainText = "avs-82k0.quv1k-5"}}, contactName = "(\1103086\1105553H/", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000700000000")))}

testObject_Contact_user_11 :: Contact
testObject_Contact_user_11 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000700000004"))), qDomain = Domain {_domainText = "156y.t.qxp-y26x"}}, contactName = "+\DC4\1063683<", contactColorId = Just 6, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000600000004")))}

testObject_Contact_user_12 :: Contact
testObject_Contact_user_12 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000003"))), qDomain = Domain {_domainText = "d2wnzbn.8.k2d4-103"}}, contactName = "l\DC1\ETB`\ETX", contactColorId = Just (-4), contactHandle = Just "", contactTeam = Nothing}

testObject_Contact_user_13 :: Contact
testObject_Contact_user_13 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000800000006"))), qDomain = Domain {_domainText = "902cigj.v2t56"}}, contactName = "\SYN\1030541\v8z", contactColorId = Just (-3), contactHandle = Just "E\EM\US[58", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000000000005")))}

testObject_Contact_user_14 :: Contact
testObject_Contact_user_14 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000300000006"))), qDomain = Domain {_domainText = "6z.ml.80ps6j5r.l"}}, contactName = "7", contactColorId = Just (-2), contactHandle = Just "h\CAN", contactTeam = Just (Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000700000008")))}

testObject_Contact_user_15 :: Contact
testObject_Contact_user_15 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), qDomain = Domain {_domainText = "739.e-h8g"}}, contactName = "U6\ESC*\SO", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000800000006")))}

testObject_Contact_user_16 :: Contact
testObject_Contact_user_16 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000500000006"))), qDomain = Domain {_domainText = "t82.x5i8-i"}}, contactName = "l", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000200000007")))}

testObject_Contact_user_17 :: Contact
testObject_Contact_user_17 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000700000002"))), qDomain = Domain {_domainText = "o5b0hrjp3x0b96.v1gxp3"}}, contactName = "fI\8868\&3z", contactColorId = Nothing, contactHandle = Just "3", contactTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000000000001")))}

testObject_Contact_user_18 :: Contact
testObject_Contact_user_18 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000800000006"))), qDomain = Domain {_domainText = "72n2x7x0.ztb0s51"}}, contactName = "\"jC\74801\144577\DC2", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000007")))}

testObject_Contact_user_19 :: Contact
testObject_Contact_user_19 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000700000007"))), qDomain = Domain {_domainText = "h664l.dio6"}}, contactName = "I", contactColorId = Just (-1), contactHandle = Just "\"7\ACK!", contactTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000000000003")))}

testObject_Contact_user_20 :: Contact
testObject_Contact_user_20 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000500000001"))), qDomain = Domain {_domainText = "pam223.b6"}}, contactName = "|K\n\n\t", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Nothing}
