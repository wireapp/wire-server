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
testObject_User_1 :: User
testObject_User_1 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), qDomain = Domain {_domainText = "2t02.41w9.d1mfq.b53"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "j}\ETB\1082108^\1104371$wUan\SO!B\ETX7\61128F\1058943\EOT\120944\1093594D+\27023"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.LG, lCountry = Just (Country {fromCountry = IM})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), userHandle = Just (Handle {fromHandle = ".04.nh04e-zx_2wo6yr6ns3854amg6c3bv8a5l4hab-jh3944n"}), userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), userManagedBy = ManagedByScim}
testObject_User_2 :: User
testObject_User_2 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), qDomain = Domain {_domainText = "tx9.o"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "", emailDomain = ""})), userDisplayName = Name {fromName = ",#=@GS.\SYNk\1113144v|#\DC1\ak\td\DC1M\1106453|\SOHj`6G\NAK4*\1099931\NAK5w\GS\vC\b\1047596p#\986131dB:`60\SIM\ETB\97076\r\63752Y\1001896xLtU\147661`\SUB\GSQ\v;\160950i\DC4.\t\NUL\DC3\1072293\ETXJN\ESCP\142818Y'G\CAND"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.EO, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "51j.uf3m"}), userExpire = Nothing, userTeam = Nothing, userManagedBy = ManagedByScim}
testObject_User_3 :: User
testObject_User_3 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), qDomain = Domain {_domainText = "46k58-j375-34.v153"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+266275170235194"})), userDisplayName = Name {fromName = "wG\SI\1067575\174231z\ENQ\DEL\STXcY *W.\1009527\SOH~]\1059956\1095042\163229\177006\98442\146701\161053[\165935br\1045443\1087133e!\b7\1012516\FS}\DC1\65837\&3\FS\1107121\1106205\ENQ\USnF\1092190\NUL(\"V\183993\DEL\1001022^\162407[\95166t\1098500`:@\21606j"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.SK, lCountry = Just (Country {fromCountry = RE})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "s70rr0uazv"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T14:59:35.077Z")), userTeam = Nothing, userManagedBy = ManagedByScim}
testObject_User_4 :: User
testObject_User_4 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), qDomain = Domain {_domainText = "ev04d6.w9nzx.j.e"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "|\GS\1078962\SI\"\DC3w\1047636VH\SUBQ\49708\996795\ACK\SO\STXi-\SI\988763\n8\SOO\ENQ,uvE\SO\164151\1107425\NUL0\22174B\68824\33925j\1008700\47452\163661\1068614\46423*?\42490HFC\1102770\&9P\1105753V\1067073\1094026B\156459e#?i\a\NAK`^\STX\1054535\n\1071317\SI\NAK:\FS\62867q/\1027625nHg\DLE\159342NWr\CANZ^|5"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = 2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.IT, lCountry = Nothing}, userService = Nothing, userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T13:34:10.342Z")), userTeam = Nothing, userManagedBy = ManagedByWire}
testObject_User_5 :: User
testObject_User_5 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "c.d8.k8128"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "%F>\175768\DLE*\n\"\17098%>/z%N\1067988\&3\DELfL7;*\vZ\1097381b\GS\GS\1010545!\STX\1045942OgL\DC2\SUB\ESC\1070215\SYN6M<F^K\28231\1074606H\1052794\&2\96997P%y\GS7\983720eb\62572\SYN)\1101967\992380\137786\1085140\453\14570Tt\1062635\1090330k\1042490\1058770\ESC\ESCw\997420E\\@\fvm\166733\a\EOTsmn0h|\bPTz\165009b\a\DC2T_QTmsTP\1031636H\1033882\1098274?\174651\1060107}\1082669:"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "Z" (Nothing))], userAccentId = ColourId {fromColourId = 2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.AV, lCountry = Just (Country {fromCountry = GD})}, userService = Nothing, userHandle = Just (Handle {fromHandle = "kyn"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T18:54:31.351Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), userManagedBy = ManagedByWire}
