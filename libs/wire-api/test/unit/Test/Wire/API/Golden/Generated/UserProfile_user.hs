{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserProfile_user where

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
testObject_UserProfile_user_1 :: UserProfile
testObject_UserProfile_user_1 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), qDomain = Domain {_domainText = "faa.e9.iz1-jr0ur"}}, profileName = Name {fromName = "uf\ETX\GS\ETB4\f\SIt\65326f,\59637\DLE\184211\&1\STXXr\EOTc\ESCh\SUB\54194\1045435\NAK\DEL\DC4\US\1007502Z\132897\1062262@WK\FSQ\983695\1096844.bGLc\b\1032566#:\997583\DC1\ESC1\998117E62\fKo\1078740C\SUB\"t\CAN;X<\SOH\996868(\DC3\1057138O\DC2Dj|\SO\1107822\31859X5\1084584y\SOH\51070*\170870\&4;'e\FSEq\1102367"}, profilePict = Pict {fromPict = []}, profileAssets = [], profileAccentId = ColourId {fromColourId = -2}, profileDeleted = True, profileService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), profileHandle = Just (Handle {fromHandle = "ru-1igaaon1rlyyj_wca92vhsunudzearminqiklxdza_nsp7ibfm_-jlp.mpq_65929vxw.x75bkh9zvgzjxmi-ivgo-7z7aucul7c30x69so-hqid6pixtxuf-bcnnqqzw.ixdkcjd2gp8vtib_fuotbvd2-rb4o.6vu6.y2fcryqyrap3l5sc97o3_bypohkzlh2.5ukmz7b9s-ge5"}), profileLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BM, lCountry = Just (Country {fromCountry = CI})}), profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T04:56:50.605Z")), profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), profileEmail = Nothing}
testObject_UserProfile_user_2 :: UserProfile
testObject_UserProfile_user_2 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "7v--o.f-k"}}, profileName = Name {fromName = " \SOHL7\26379Ay-\985498\USCe@\1000342\b\30464@a23\999310\1062157rP{OEM\NAK\t\132361=tH\994446k\986089S\SO\SI\vg{w\1038219 \ESCNwmLN~tmZ%1qA?_\\\1000257\US\67113\&4q\EMi|\DC2\1044497\1081827\ACK\1102616\fr`|\NAKgJ\1068049\DC3\48131\1091152+\1012177Qb*"}, profilePict = Pict {fromPict = []}, profileAssets = [], profileAccentId = ColourId {fromColourId = -2}, profileDeleted = True, profileService = Nothing, profileHandle = Nothing, profileLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.AS, lCountry = Just (Country {fromCountry = UM})}), profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T16:53:23.006Z")), profileTeam = Nothing, profileEmail = Just (Email {emailLocal = "\NULK", emailDomain = ""})}
testObject_UserProfile_user_3 :: UserProfile
testObject_UserProfile_user_3 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "8.xq--95bq29d"}}, profileName = Name {fromName = "m:\SUB\\\1037887\986794u\144988\162822En\DC3V|\rM\ENQ\1059845\&5\2237y(\EMPSl\1032313Nu\135877DZ\30472`\1013272\1053237\ENQ\986415\RS\RS\n\ETB4\137391UJo\164924\1093545LU$\984717\42416\r\DEL\STXl5\ETX\133887\1074481d\GS\EM\tsT\62987+\r:\1028288XK\1093042e\178229\&9w8L"}, profilePict = Pict {fromPict = []}, profileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing))], profileAccentId = ColourId {fromColourId = 2}, profileDeleted = False, profileService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), profileHandle = Just (Handle {fromHandle = "jpwfumhgiq5hjznj5-uu97ith.mncuo495pvpgd_iy.cb35q8drebu563wr2rsfeonmaecvr862mj0kvb-40hvnio55mxu"}), profileLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KJ, lCountry = Just (Country {fromCountry = PA})}), profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T17:21:33.896Z")), profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), profileEmail = Just (Email {emailLocal = "\8369", emailDomain = "Z\""})}
testObject_UserProfile_user_4 :: UserProfile
testObject_UserProfile_user_4 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "a--x4.5di6.kx453-ga.6.rj3"}}, profileName = Name {fromName = "V\DC4\NUL|7\\\DLEBUxc\NAKxi\1006894\1028796\4273\33367VQB\1021739\120901j\1058002\ESC{;N\1054513\74891U)\1040947`X\US\1031045!sZJw\\\STX\173718\984271~}:\1107427\1054133\vZY\r@\1049691\154864\134277\4377\1036481I%\1090626w\1008000[l\1005534\\w:9eu$.@5\50393\1065886\1037829l[?\65321\1041328\DELc\SOH\172535\FS\EM{:eD\98577\54805:\53031\SUB]11p::{\162760\147880\GSU\989283M\ENQ\997378\STX!\54893Soc0.0l"}, profilePict = Pict {fromPict = []}, profileAssets = [], profileAccentId = ColourId {fromColourId = 2}, profileDeleted = False, profileService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), profileHandle = Just (Handle {fromHandle = "na0"}), profileLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.JA, lCountry = Just (Country {fromCountry = BV})}), profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T09:33:07.788Z")), profileTeam = Nothing, profileEmail = Just (Email {emailLocal = "KL", emailDomain = ""})}
testObject_UserProfile_user_5 :: UserProfile
testObject_UserProfile_user_5 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), qDomain = Domain {_domainText = "wm12.f6-2-a-rw"}}, profileName = Name {fromName = "q<-\SYN\985122\74278rH\1109874\SUBN\6534ja{c\43919.\NUL\SYN\136870\EM3n\132566\1110259b\DELA\1040574+\EOT\EOT,\b\1107065\1108366\1024173W-=\NAK<=@1SHmK1tc\12358\f\1021546Ks\ETX\b\1058249\FSm1b\154436Y`P\CAN\1027321\RS>DYs\\B\1012783HV\vT\137347\43303"}, profilePict = Pict {fromPict = []}, profileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete))], profileAccentId = ColourId {fromColourId = 1}, profileDeleted = False, profileService = Nothing, profileHandle = Just (Handle {fromHandle = "vdx2bud"}), profileLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.LU, lCountry = Just (Country {fromCountry = PA})}), profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T09:08:22.107Z")), profileTeam = Nothing, profileEmail = Just (Email {emailLocal = "", emailDomain = "\DC1e"})}
