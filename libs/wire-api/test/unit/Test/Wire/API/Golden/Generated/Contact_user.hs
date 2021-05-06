{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Contact_user where
import Data.Domain ( Domain(Domain, _domainText) )
import Data.Id ( Id(Id) )
import Data.Qualified
    ( Qualified(Qualified, qUnqualified, qDomain) )
import Imports ( Maybe(Nothing, Just), fromJust )
import qualified Data.UUID as UUID ( fromString )
import Wire.API.User.Search ( Contact(..) )

testObject_Contact_user_1 :: Contact
testObject_Contact_user_1 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000008"))), qDomain = Domain {_domainText = "2-1j--f9.h53"}}, contactName = "", contactColorId = Just (-3), contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000600000003")))}
testObject_Contact_user_2 :: Contact
testObject_Contact_user_2 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000800000002"))), qDomain = Domain {_domainText = "6.3.25e.mfb28.21.f.r758.oj1"}}, contactName = "wX", contactColorId = Nothing, contactHandle = Just "\1057893\DLE\v\757<]", contactTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000600000003")))}
testObject_Contact_user_3 :: Contact
testObject_Contact_user_3 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000800000000"))), qDomain = Domain {_domainText = "4vn.bo0"}}, contactName = "\1065993A", contactColorId = Nothing, contactHandle = Just "\1005786a2/6", contactTeam = Nothing}
testObject_Contact_user_4 :: Contact
testObject_Contact_user_4 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0007-0000-000400000007"))), qDomain = Domain {_domainText = "r2c.q4"}}, contactName = "\1043998v\n\144939*", contactColorId = Just (-3), contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000600000006")))}
testObject_Contact_user_5 :: Contact
testObject_Contact_user_5 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0007-0000-000400000008"))), qDomain = Domain {_domainText = "5igyf.6091-z80hxiqq5n0c12--nx.8-48-hs1.9-k--ei7.zo2p0524"}}, contactName = "(F\147647J!", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Nothing}
testObject_Contact_user_6 :: Contact
testObject_Contact_user_6 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000600000005"))), qDomain = Domain {_domainText = "js.rdu-b2"}}, contactName = "N\1042253", contactColorId = Just (-6), contactHandle = Just "\43990S\995332(", contactTeam = Nothing}
testObject_Contact_user_7 :: Contact
testObject_Contact_user_7 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000006-0000-0003-0000-000000000006"))), qDomain = Domain {_domainText = "a2n-6-n.oqsh.e"}}, contactName = "\26582R", contactColorId = Just (-2), contactHandle = Just "/ck{\CAN", contactTeam = Just (Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000200000000")))}
testObject_Contact_user_8 :: Contact
testObject_Contact_user_8 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000500000003"))), qDomain = Domain {_domainText = "0-z.9.y-pag2d80.z"}}, contactName = "\1012535\1109025\&9\1100318\SYN", contactColorId = Just 1, contactHandle = Just "", contactTeam = Nothing}
testObject_Contact_user_9 :: Contact
testObject_Contact_user_9 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000400000003"))), qDomain = Domain {_domainText = "md90h.2ay8.s-7k.b7d5h.if3.t9.p-1-c2"}}, contactName = ">\51083/\159784'", contactColorId = Just 5, contactHandle = Nothing, contactTeam = Nothing}
testObject_Contact_user_10 :: Contact
testObject_Contact_user_10 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000800000005"))), qDomain = Domain {_domainText = "507bs8-i.s-o9u.u74idza"}}, contactName = "c\999852", contactColorId = Just 2, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000400000004")))}
testObject_Contact_user_11 :: Contact
testObject_Contact_user_11 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000600000000"))), qDomain = Domain {_domainText = "33-42su72.v6e-l7dx"}}, contactName = "<n\DC2,+|", contactColorId = Nothing, contactHandle = Just "\as", contactTeam = Just (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000600000007")))}
testObject_Contact_user_12 :: Contact
testObject_Contact_user_12 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "7e4zp.6u147-ra2-e.xn"}}, contactName = "\1034691\1108510M", contactColorId = Just (-6), contactHandle = Just "^\1060788\1071708", contactTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000500000004")))}
testObject_Contact_user_13 :: Contact
testObject_Contact_user_13 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0007-0000-000300000008"))), qDomain = Domain {_domainText = "ug3i6o88.a70.111.wz2t.40-9g754.v1-bz.4.ks88c--2d85"}}, contactName = "\989958\r\1054994", contactColorId = Just (-3), contactHandle = Nothing, contactTeam = Nothing}
testObject_Contact_user_14 :: Contact
testObject_Contact_user_14 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000300000001"))), qDomain = Domain {_domainText = "c-p-izlh21.419.86j.4-i-qd9.emp.z2"}}, contactName = "\SOH", contactColorId = Just 5, contactHandle = Just "\SO", contactTeam = Nothing}
testObject_Contact_user_15 :: Contact
testObject_Contact_user_15 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000600000001"))), qDomain = Domain {_domainText = "pxm8n5.iey.y.ie6"}}, contactName = "\SYN", contactColorId = Just 2, contactHandle = Just "\SOH_\1098256\f\SOH9", contactTeam = Just (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000500000001")))}
testObject_Contact_user_16 :: Contact
testObject_Contact_user_16 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000007-0000-0007-0000-000400000008"))), qDomain = Domain {_domainText = "1.9yiwd-r.p-5pt.5i.ygb2"}}, contactName = "\CANX8\145327\ENQ", contactColorId = Just (-4), contactHandle = Just "\v\",,)", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000700000000")))}
testObject_Contact_user_17 :: Contact
testObject_Contact_user_17 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000004"))), qDomain = Domain {_domainText = "l.o6-lu"}}, contactName = "\SOHD<q\7952W", contactColorId = Just 2, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000800000002")))}
testObject_Contact_user_18 :: Contact
testObject_Contact_user_18 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000700000004"))), qDomain = Domain {_domainText = "n-vk.ql5xsv-a"}}, contactName = "\98688~", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000600000005")))}
testObject_Contact_user_19 :: Contact
testObject_Contact_user_19 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000300000005"))), qDomain = Domain {_domainText = "7cq-q8.tv662"}}, contactName = "\1028794\137311'}r", contactColorId = Just (-4), contactHandle = Nothing, contactTeam = Nothing}
testObject_Contact_user_20 :: Contact
testObject_Contact_user_20 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000100000003"))), qDomain = Domain {_domainText = "9.n1ab6o.u785051f473"}}, contactName = "%8j", contactColorId = Just 5, contactHandle = Just ")6\\/`\ETB", contactTeam = Nothing}
