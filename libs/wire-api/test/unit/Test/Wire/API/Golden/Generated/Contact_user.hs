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
testObject_Contact_user_1 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000400000006"))), qDomain = Domain {_domainText = "5y-l698t.8-9--ma20.s5y0l-b0"}}, contactName = "'\a^,", contactColorId = Just (-4), contactHandle = Just "\1012619\ACK", contactTeam = Just (Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000800000003")))}
testObject_Contact_user_2 :: Contact
testObject_Contact_user_2 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000800000000"))), qDomain = Domain {_domainText = "l.hjqs9h"}}, contactName = "p\166878\&9[P", contactColorId = Just 3, contactHandle = Just "\1082335\NUL\32710", contactTeam = Nothing}
testObject_Contact_user_3 :: Contact
testObject_Contact_user_3 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000007"))), qDomain = Domain {_domainText = "x.o-06"}}, contactName = "G^(\NULz", contactColorId = Just (-3), contactHandle = Just "\20862\128705+", contactTeam = Just (Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000800000001")))}
testObject_Contact_user_4 :: Contact
testObject_Contact_user_4 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000000000007"))), qDomain = Domain {_domainText = "114.s07.vkyf"}}, contactName = "@", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000008")))}
testObject_Contact_user_5 :: Contact
testObject_Contact_user_5 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000500000000"))), qDomain = Domain {_domainText = "iw5zk8.c-4-g2l5w68z--6y"}}, contactName = "@\1106871", contactColorId = Just 4, contactHandle = Just "R", contactTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000200000007")))}
testObject_Contact_user_6 :: Contact
testObject_Contact_user_6 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000600000002"))), qDomain = Domain {_domainText = "ja-ih.73ms.2.tz67152p61h"}}, contactName = "h3`\207", contactColorId = Just 1, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000500000002")))}
testObject_Contact_user_7 :: Contact
testObject_Contact_user_7 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000400000001"))), qDomain = Domain {_domainText = "q9.kc7-3c"}}, contactName = "\SO,\29909*4&", contactColorId = Nothing, contactHandle = Just "\DC1", contactTeam = Just (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000500000007")))}
testObject_Contact_user_8 :: Contact
testObject_Contact_user_8 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000800000006"))), qDomain = Domain {_domainText = "rs0.2-5.d7"}}, contactName = "EH", contactColorId = Just (-4), contactHandle = Just "Z\149880\bx44", contactTeam = Nothing}
testObject_Contact_user_9 :: Contact
testObject_Contact_user_9 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000100000001"))), qDomain = Domain {_domainText = "z.4s8td5.l4iy-a9l"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000007")))}
testObject_Contact_user_10 :: Contact
testObject_Contact_user_10 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000100000001"))), qDomain = Domain {_domainText = "kc4-0.w0.h"}}, contactName = "~\58462|\991917C", contactColorId = Just (-6), contactHandle = Just ")", contactTeam = Nothing}
testObject_Contact_user_11 :: Contact
testObject_Contact_user_11 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000006"))), qDomain = Domain {_domainText = "n4.v7"}}, contactName = "Q*\186777\40870&\998321", contactColorId = Just (-5), contactHandle = Just "g", contactTeam = Nothing}
testObject_Contact_user_12 :: Contact
testObject_Contact_user_12 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000600000006"))), qDomain = Domain {_domainText = "dwo.l8a"}}, contactName = "", contactColorId = Just (-4), contactHandle = Nothing, contactTeam = Nothing}
testObject_Contact_user_13 :: Contact
testObject_Contact_user_13 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000200000005"))), qDomain = Domain {_domainText = "7.q"}}, contactName = "\1037786*%+\185256J", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Nothing}
testObject_Contact_user_14 :: Contact
testObject_Contact_user_14 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000007"))), qDomain = Domain {_domainText = "vk-au.69o3ph35vc23i.3s52g.9.wk7"}}, contactName = "MYli.K", contactColorId = Just (-2), contactHandle = Just "\t", contactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0007-0000-000400000001")))}
testObject_Contact_user_15 :: Contact
testObject_Contact_user_15 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000500000004"))), qDomain = Domain {_domainText = "hr-zu-i-24i.57-zr-2u.wkutm"}}, contactName = "RU\1108246\64122\54035", contactColorId = Just 2, contactHandle = Just "\1095686\1061633n\997580,", contactTeam = Nothing}
testObject_Contact_user_16 :: Contact
testObject_Contact_user_16 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000000000007"))), qDomain = Domain {_domainText = "7.427k-zr-i.xjwlc"}}, contactName = "", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0006-0000-000600000008")))}
testObject_Contact_user_17 :: Contact
testObject_Contact_user_17 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000600000004"))), qDomain = Domain {_domainText = "7lurvdy4s0p4l7o.o5"}}, contactName = "!\DLE", contactColorId = Just (-2), contactHandle = Just "\SUB~K\NUL\1101195\v", contactTeam = Just (Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000800000007")))}
testObject_Contact_user_18 :: Contact
testObject_Contact_user_18 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000800000002"))), qDomain = Domain {_domainText = "3oy.1k4.k.f-z"}}, contactName = "", contactColorId = Just 6, contactHandle = Just "Zd\29151\STX\RSh", contactTeam = Just (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000100000002")))}
testObject_Contact_user_19 :: Contact
testObject_Contact_user_19 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000600000000"))), qDomain = Domain {_domainText = "8.sp2ww"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "\DC1-\17031I:\ENQ", contactTeam = Nothing}
testObject_Contact_user_20 :: Contact
testObject_Contact_user_20 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000700000001"))), qDomain = Domain {_domainText = "6s5r.n0c-1jv7q"}}, contactName = "g%ON\1077828", contactColorId = Just 4, contactHandle = Just "\1033469I\98631F\1080139", contactTeam = Nothing}
