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
testObject_Contact_user_1 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000400000008"))), qDomain = Domain {_domainText = "21-x6b8h.g"}}, contactName = "\r\36824\US\1024047{c", contactColorId = Just (-4), contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000100000006")))}
testObject_Contact_user_2 :: Contact
testObject_Contact_user_2 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000100000008"))), qDomain = Domain {_domainText = "2-9e.3a.v-29n4l2"}}, contactName = "", contactColorId = Just 1, contactHandle = Just "}", contactTeam = Just (Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000700000001")))}
testObject_Contact_user_3 :: Contact
testObject_Contact_user_3 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000800000002"))), qDomain = Domain {_domainText = "in2qd33.k4009--jp"}}, contactName = "n\SUBL\1026800", contactColorId = Just (-2), contactHandle = Just "~\EOT\1091826I\1016038", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000100000003")))}
testObject_Contact_user_4 :: Contact
testObject_Contact_user_4 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000800000008"))), qDomain = Domain {_domainText = "98z-2p.uft.w.q68.9385-4x.k8"}}, contactName = ",sYE\1018792", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000300000004")))}
testObject_Contact_user_5 :: Contact
testObject_Contact_user_5 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000600000001"))), qDomain = Domain {_domainText = "vm.k--20f5fgmy1p1805"}}, contactName = "{0\21889\EM\DC2\1062824", contactColorId = Nothing, contactHandle = Just "M*\\9", contactTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000100000001")))}
testObject_Contact_user_6 :: Contact
testObject_Contact_user_6 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000800000004"))), qDomain = Domain {_domainText = "1r.qwk.kp1173s4l117s4"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "\GS*z\1075381", contactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000500000000")))}
testObject_Contact_user_7 :: Contact
testObject_Contact_user_7 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000400000005"))), qDomain = Domain {_domainText = "ak16.g679.k.ug-q-89.f.6-10y-6.ygm4"}}, contactName = "\DLE\CAN$v", contactColorId = Just (-4), contactHandle = Just "", contactTeam = Nothing}
testObject_Contact_user_8 :: Contact
testObject_Contact_user_8 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000007-0000-0006-0000-000000000006"))), qDomain = Domain {_domainText = "n0-5rn-n0.i-phpg-7-541i"}}, contactName = "\188838", contactColorId = Just 4, contactHandle = Just "5\SUB6u\USB", contactTeam = Nothing}
testObject_Contact_user_9 :: Contact
testObject_Contact_user_9 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000600000007"))), qDomain = Domain {_domainText = "9.t--y"}}, contactName = "%", contactColorId = Just 3, contactHandle = Just "5\141585", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000000000006")))}
testObject_Contact_user_10 :: Contact
testObject_Contact_user_10 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000500000004"))), qDomain = Domain {_domainText = "b8507o8--1.k5kd2t"}}, contactName = "2\CAN\986341\1069910", contactColorId = Just (-1), contactHandle = Just "e\99866\985898KI@", contactTeam = Nothing}
testObject_Contact_user_11 :: Contact
testObject_Contact_user_11 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000600000003"))), qDomain = Domain {_domainText = "40vp.39i4.6dr.p9x.83o.e5f4"}}, contactName = "", contactColorId = Just (-2), contactHandle = Just "\DC3)9\990390d\96194", contactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0007-0000-000300000007")))}
testObject_Contact_user_12 :: Contact
testObject_Contact_user_12 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000100000000"))), qDomain = Domain {_domainText = "rm.q"}}, contactName = "[\SUB", contactColorId = Nothing, contactHandle = Just "\DC2\169951\US", contactTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000700000002")))}
testObject_Contact_user_13 :: Contact
testObject_Contact_user_13 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000300000006"))), qDomain = Domain {_domainText = "zv2be.u4p2"}}, contactName = "]\1045914\&6+", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000700000002")))}
testObject_Contact_user_14 :: Contact
testObject_Contact_user_14 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000000000002"))), qDomain = Domain {_domainText = "1ox.56.xj2"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "\\", contactTeam = Just (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000700000007")))}
testObject_Contact_user_15 :: Contact
testObject_Contact_user_15 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000007-0000-0007-0000-000300000007"))), qDomain = Domain {_domainText = "9ra.8.t56-51---yf2.1zwn40866.t7"}}, contactName = "W", contactColorId = Just 6, contactHandle = Just "?\1010430\r", contactTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000200000003")))}
testObject_Contact_user_16 :: Contact
testObject_Contact_user_16 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000000"))), qDomain = Domain {_domainText = "f4.v"}}, contactName = "\ENQ", contactColorId = Just (-3), contactHandle = Just "$\36976F[d\1006392", contactTeam = Just (Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000100000004")))}
testObject_Contact_user_17 :: Contact
testObject_Contact_user_17 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "tn6omcir7v2-i.uj48e"}}, contactName = "P", contactColorId = Just (-1), contactHandle = Just ",Zpg1\168686", contactTeam = Just (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000800000007")))}
testObject_Contact_user_18 :: Contact
testObject_Contact_user_18 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000200000000"))), qDomain = Domain {_domainText = "2x-sn.w94-9"}}, contactName = "\47583*", contactColorId = Nothing, contactHandle = Just "%o\1005901s", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000200000002")))}
testObject_Contact_user_19 :: Contact
testObject_Contact_user_19 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0007-0000-000500000007"))), qDomain = Domain {_domainText = "b2-5.122934k37rm--9.r94"}}, contactName = "\1009232\1012468\989501", contactColorId = Just (-2), contactHandle = Just "s", contactTeam = Just (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000700000008")))}
testObject_Contact_user_20 :: Contact
testObject_Contact_user_20 = Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000100000004"))), qDomain = Domain {_domainText = "p-k548.jt"}}, contactName = "2n\SO\"\DLE\133101", contactColorId = Just (-3), contactHandle = Just "\1024708\12294>)", contactTeam = Nothing}
