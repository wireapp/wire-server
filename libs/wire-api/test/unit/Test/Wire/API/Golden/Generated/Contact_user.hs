{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Contact_user where

import Codec.MIME.Type (Type(..))
import qualified Codec.MIME.Type as MIME
import Control.Lens ((.~))
import Data.Code
import Data.Coerce
import Data.Currency
import Data.Domain
import Data.Handle
import Data.Id
import Data.ISO3166_CountryCodes
import Data.Json.Util
import Data.List1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc
import Data.PEM
import Data.Qualified
import Data.Range (unsafeRange)
import qualified Data.Set as Set
import Data.Text.Ascii
import Data.Time (secondsToNominalDiffTime)
import Imports hiding (LT, GT)
import qualified Data.LanguageCodes
import qualified Data.UUID as UUID
import Test.Tasty (testGroup, TestTree)
import URI.ByteString
import qualified Wire.API.Call.Config as CallConfig
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider
import qualified Wire.API.Provider.External as Provider
import qualified Wire.API.Provider.Service as Provider
import qualified Wire.API.Provider.Service.Tag as Provider
import Data.Aeson
import GHC.Exts
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
-- import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
-- import Wire.API.Provider.Service.Tag
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
-- import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
testObject_Contact_user_1 :: Contact
testObject_Contact_user_1 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000200000001"))), qDomain = Domain {_domainText = "4--7.m-6xh"}}, contactName = "\1084084{F", contactColorId = Just (-3), contactHandle = Just "\\\1005734|3", contactTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000100000005")))}
testObject_Contact_user_2 :: Contact
testObject_Contact_user_2 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000500000005"))), qDomain = Domain {_domainText = "e442.mb4"}}, contactName = "\r\SI", contactColorId = Just (-4), contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000700000006")))}
testObject_Contact_user_3 :: Contact
testObject_Contact_user_3 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000100000007"))), qDomain = Domain {_domainText = "hg.q15"}}, contactName = "aq", contactColorId = Nothing, contactHandle = Just "\1031788+Gzz", contactTeam = Nothing}
testObject_Contact_user_4 :: Contact
testObject_Contact_user_4 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000400000000"))), qDomain = Domain {_domainText = "84v.l445l"}}, contactName = "\1111492l$\"w", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Nothing}
testObject_Contact_user_5 :: Contact
testObject_Contact_user_5 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000500000004"))), qDomain = Domain {_domainText = "67lr.c4bx7fq57.sjbnlx"}}, contactName = "@j", contactColorId = Just 2, contactHandle = Just "Z", contactTeam = Nothing}
testObject_Contact_user_6 :: Contact
testObject_Contact_user_6 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000700000006"))), qDomain = Domain {_domainText = "2jv6t9-n13.2l2y1.4.92of.awey.cmh-k4"}}, contactName = "Ul\"", contactColorId = Just 1, contactHandle = Just "]", contactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000400000005")))}
testObject_Contact_user_7 :: Contact
testObject_Contact_user_7 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000800000008"))), qDomain = Domain {_domainText = "m4-n8on.u8j8-y-cbbhcv"}}, contactName = "\1030067\ETXA", contactColorId = Just (-2), contactHandle = Just "\1025147\1009073\&6", contactTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000000000005")))}
testObject_Contact_user_8 :: Contact
testObject_Contact_user_8 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000800000007"))), qDomain = Domain {_domainText = "76-11qt36-z.57nb.8aum0j.g"}}, contactName = "\biJ\voj", contactColorId = Just 6, contactHandle = Just "", contactTeam = Nothing}
testObject_Contact_user_9 :: Contact
testObject_Contact_user_9 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000500000002"))), qDomain = Domain {_domainText = "0n3c0e.ju"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "\GS'\v\179604", contactTeam = Just (Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000000000004")))}
testObject_Contact_user_10 :: Contact
testObject_Contact_user_10 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000400000003"))), qDomain = Domain {_domainText = "lkjpm37.4y.o"}}, contactName = "S\SO\165057", contactColorId = Just 0, contactHandle = Just "=f", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000600000004")))}
testObject_Contact_user_11 :: Contact
testObject_Contact_user_11 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000200000008"))), qDomain = Domain {_domainText = "y7gs80.p-d40.f"}}, contactName = "\SOH\EOT", contactColorId = Just 1, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000600000001")))}
testObject_Contact_user_12 :: Contact
testObject_Contact_user_12 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000000000006"))), qDomain = Domain {_domainText = "1.7j.r76m"}}, contactName = "", contactColorId = Just (-3), contactHandle = Just "E*\DC2", contactTeam = Nothing}
testObject_Contact_user_13 :: Contact
testObject_Contact_user_13 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000001"))), qDomain = Domain {_domainText = "m-tg.ypw8n"}}, contactName = "D", contactColorId = Just (-4), contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000100000001")))}
testObject_Contact_user_14 :: Contact
testObject_Contact_user_14 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000100000007"))), qDomain = Domain {_domainText = "7bq.n7982pzj0y6w5h9--69m44"}}, contactName = "V", contactColorId = Just 4, contactHandle = Just "@", contactTeam = Just (Id (fromJust (UUID.fromString "00000007-0000-0001-0000-000200000007")))}
testObject_Contact_user_15 :: Contact
testObject_Contact_user_15 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000800000008"))), qDomain = Domain {_domainText = "c645.os59"}}, contactName = "\1092125U", contactColorId = Nothing, contactHandle = Just "1\ENQ", contactTeam = Just (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000300000008")))}
testObject_Contact_user_16 :: Contact
testObject_Contact_user_16 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000600000006"))), qDomain = Domain {_domainText = "v8z7.z049"}}, contactName = "\DEL", contactColorId = Just 5, contactHandle = Just "?", contactTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000400000001")))}
testObject_Contact_user_17 :: Contact
testObject_Contact_user_17 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000000000002"))), qDomain = Domain {_domainText = "6i-m.k467xl4k-gr"}}, contactName = "\f\DC1\1078650", contactColorId = Nothing, contactHandle = Just "\n\179476\1063645R", contactTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000000000002")))}
testObject_Contact_user_18 :: Contact
testObject_Contact_user_18 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000300000005"))), qDomain = Domain {_domainText = "nx3.yg.s8s-x.r-uo5h4v"}}, contactName = "A", contactColorId = Just (-1), contactHandle = Just "G\55265\13301\&4", contactTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000700000003")))}
testObject_Contact_user_19 :: Contact
testObject_Contact_user_19 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000200000001"))), qDomain = Domain {_domainText = "e65.28.l-f1.6.cuo7.70zz7q293.55gax.ji0"}}, contactName = "TpvQ9", contactColorId = Just 5, contactHandle = Just "y\DC2\1035236", contactTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000200000005")))}
testObject_Contact_user_20 :: Contact
testObject_Contact_user_20 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000600000008"))), qDomain = Domain {_domainText = "g-853l.xpk"}}, contactName = "F", contactColorId = Just 4, contactHandle = Just "", contactTeam = Nothing}
