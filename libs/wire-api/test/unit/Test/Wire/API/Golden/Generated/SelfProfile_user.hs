{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.SelfProfile_user where

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
testObject_SelfProfile_1 :: SelfProfile
testObject_SelfProfile_1 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), qDomain = Domain {_domainText = "t4j6.y"}}, userIdentity = Just (SSOIdentity (UserSSOId "" "") (Just (Email {emailLocal = "", emailDomain = ""})) (Just (Phone {fromPhone = "+91656405614107"}))), userDisplayName = Name {fromName = "\142381Q I\1059467IibF\1071625\ENQ\149210-;ze>\12804ZJ\EM\FS\202l\1016435F3b:\1054014\1010983\b[\n\1111028"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = -1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.BS, lCountry = Just (Country {fromCountry = GF})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}), userHandle = Just (Handle {fromHandle = "1zt3480gy."}), userExpire = Nothing, userTeam = Nothing, userManagedBy = ManagedByScim}}
testObject_SelfProfile_2 :: SelfProfile
testObject_SelfProfile_2 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "g-y7.9o7.x6f"}}, userIdentity = Just (SSOIdentity (UserSSOId "" "") (Just (Email {emailLocal = "", emailDomain = ""})) Nothing), userDisplayName = Name {fromName = "\1104016{\FS>P\1000518N\NAK\DC1\NAKf\r(\v\US\1048745\\&n\CANY\ETB-<6n'x\NAKsty\DC3V\1060426\165148E\tu8\1110207\RS\152725\FS p\131796*\146324et\1073161\DEL5/\1022988X\n\tck\CAN\SYN1m1TO|_\175412\&1\SUB\""}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.DZ, lCountry = Just (Country {fromCountry = LR})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), userHandle = Just (Handle {fromHandle = "i13ncn7dunwzpk3gi5m92wsxlvb49urkdn9wwz-qgzln9.7_13ycuirf41ny50nka3c6p04cdx29"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T19:23:00.131Z")), userTeam = Nothing, userManagedBy = ManagedByScim}}
testObject_SelfProfile_3 :: SelfProfile
testObject_SelfProfile_3 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), qDomain = Domain {_domainText = "yx-7tpk9.8sj.0.8.7.v5"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "]7\1015126r\128436\&8]#\1001559\33066\1069741"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = 0}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.HZ, lCountry = Just (Country {fromCountry = DE})}, userService = Nothing, userHandle = Just (Handle {fromHandle = "-4oefhvz59"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T09:06:19.939Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), userManagedBy = ManagedByScim}}
testObject_SelfProfile_4 :: SelfProfile
testObject_SelfProfile_4 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "lq8t.oj9ixbq-z3.8u-b.y.q0"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "xv\1016026l3*Z5\71437\CAN\ENQ3\149153\SIk`~\DC3\DC1loOd'2\182178\158336\1075165\1107351uc\DC4\38215\SUB\63039\DC1\36394vt\r\139993\183328\1023837\&1\1015044;G\v}z\1067150\22529ab\NAK\7670\GSy2vU\DC4\ESC\45205qt.\EM<\1006406\172531\ETXZ\1092919s\137639Tp\178555B%\fL)_\155540@\GS\1043885h\94062D\DC1\ETX\1044707r\EM\92747(9Z0I5J\EOT\28939RlJ\1085294\150173\140020\1081660]\1090549\US\ACK"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "@" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.NR, lCountry = Nothing}, userService = Nothing, userHandle = Just (Handle {fromHandle = "ziasux60"}), userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), userManagedBy = ManagedByScim}}
testObject_SelfProfile_5 :: SelfProfile
testObject_SelfProfile_5 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), qDomain = Domain {_domainText = "86.mj6-sj5"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+80239632553812"})), userDisplayName = Name {fromName = "^\28556\59036H\1099287~\1107932\1047798\nj\1106257G\1056744^]\61501m9'\n\113714\120691\SO"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = 1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.SC, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T00:48:16.329Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))), userManagedBy = ManagedByWire}}
