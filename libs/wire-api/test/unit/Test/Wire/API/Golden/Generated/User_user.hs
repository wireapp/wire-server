{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.User_user where

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
testObject_User_user_1 :: User
testObject_User_user_1 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))), qDomain = Domain {_domainText = "qlx.r-l.r"}}, userIdentity = Just (SSOIdentity (UserScimExternalId "") (Just (Email {emailLocal = "", emailDomain = ""})) (Just (Phone {fromPhone = "+5309503459"}))), userDisplayName = Name {fromName = "N:\1021797\ACK_\DLE|t\180360\ENQ\DELm\1112307\95462\STX0\7876\ACK\ETB5\RS9\187015\SYN\f\1047002*V\170069\&2p\SUB,\STXQ\RS(\1007296\1024658I\SOH{3&\DLE\DC2\150317wj\\\1044897\RSi86P\49198\49234G\DC1\GS\STXE@\1014706\133448PJ=M\1027992Z\1033446Ah{'<\DC4;\ENQ\SUBBc\16583P\2503\&6\FS\ESC\1092676_;-[\v(w\DLE\1081967Fq\1067951\t.\n6\GSXw\18118u\188645\1098682H\42441="}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.NE, lCountry = Just (Country {fromCountry = US})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "c-n-ky_7gy98h4xh0ba7qzb1_6d0o_9li.dd7-n-kz56k19wfffrmzyushaqmqtdrm_7vftfjq42-yps9ehlkemb8f6oolvod"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T15:08:38.867Z")), userTeam = Nothing, userManagedBy = ManagedByScim}
testObject_User_user_2 :: User
testObject_User_user_2 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "11y0u2.w7o1n-8-0"}}, userIdentity = Just (SSOIdentity (UserSSOId "" "") Nothing (Just (Phone {fromPhone = "+2697103492805"}))), userDisplayName = Name {fromName = "g\33909\19219D\ETX~\1034614\1054092q\1062191\65205!:\ESC1\1009677\1111089\59995\&8j&\SOG1\168966r}HHbo!hsh\bO`\1097719[\157460\US\SUB*pC9\45983\1055896|\1074150\159252\159286\1056474\DC4\n0\59317\999369\1012513\177213[\189802(\24149 R\b\1112802"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "\4137" (Nothing))], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.KG, lCountry = Just (Country {fromCountry = MK})}, userService = Nothing, userHandle = Just (Handle {fromHandle = "84bpk6a3c6_tz8zuswlo57s1vdl3fch.rxl-4mo1sk7h-xl-8y8buotoiucgj2fhna186f-23gb-tljdr740hci9njhzq3juaapoock4efig9llk597n4hqd07cd.8fpj1ui-_ac1aein7q.7zlvuxuh11x80p.obn3ljaczew-y_373p9-03s9qj34lz.j84j7-pdt97-nc5seo8-t655hd0"}), userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), userManagedBy = ManagedByWire}
testObject_User_user_3 :: User
testObject_User_user_3 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "ov7dsi9.4r8.3s9g4049l.htewmtx.qp-x"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "\3875\&7)Uz&\SOU8\SUB"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.FI, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "h0u8k.7svmxtgsclcg6p92q91ss5vbvtpwu3cv5drfm58uhyv6o8bav-s3-ciwgxbt2wsi7p5jarsfiqa3"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T08:27:13.810Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), userManagedBy = ManagedByWire}
testObject_User_user_4 :: User
testObject_User_user_4 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "d-c5e.b-t"}}, userIdentity = Just (SSOIdentity (UserScimExternalId "") (Just (Email {emailLocal = "", emailDomain = ""})) (Just (Phone {fromPhone = "+1083024239568"}))), userDisplayName = Name {fromName = "p.'W\1020533\65428@"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.OS, lCountry = Just (Country {fromCountry = NE})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), userHandle = Nothing, userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), userManagedBy = ManagedByScim}
testObject_User_user_5 :: User
testObject_User_user_5 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), qDomain = Domain {_domainText = "0-6.4sq.ck-61"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "O", emailDomain = "\GS"})), userDisplayName = Name {fromName = "9C\1044341\RS{O\EOT\EMb\GS\164349\&3\SOH82+'\160581cS\1077578\&3\43003\FS:\DC3\58249zh\1091455\1071456\998348\&8Vn\SUB\vB_\1066166Jzs\ETB\fa+\ACK\119073XE\ETX\SOHyX\1020808\FS!N\DC3\149631@?\a\ACKR \DC4P\46661?\SYNd\999078\a^C\DEL\1078900p2\DC3#$\NAK\1028664&[m5x\b\181863\"\52740\\b\DC1W\157334\DEL7\US\136401\EOT\1041285\NAKGHH\44561\STXw\a\186144\1017385\v"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.GN, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "q0rkotv--r1l6npa1evsrsur40nk"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T14:40:48.787Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), userManagedBy = ManagedByWire}
