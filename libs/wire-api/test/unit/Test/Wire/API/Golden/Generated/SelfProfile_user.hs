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
testObject_SelfProfile_user_1 :: SelfProfile
testObject_SelfProfile_user_1 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "5.vo5q1d"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "G", emailDomain = ""}) (Phone {fromPhone = "+305218896467177"})), userDisplayName = Name {fromName = "\rH\137296/w\1005082\&9\1064087,YVy-2\DLE\101036\SIt=\1083483h]\SYN\44033\DC2\66751Eu\ETXr\FS\SUB\74827\1017623\&5\99863G\120643\128354\DLE\1008259\v\1039248j\994030\1038708GQ\990437'!{tL\ETBJt\186771\100784@\ESCmu`\1060460({yx+=Q\1059202M-=\n\1067561\99499h\994151\148434\fS\68492|a\DLE\43910\45282\152406\1056621\1033327;\1106122\1021429p\GSX\187840.Bs!&t4\164092\&6R6\189496>\36668\26026\RS\ETX\177905\ESCi\172085X"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.BE, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T23:10:47.888Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), userManagedBy = ManagedByScim}}
testObject_SelfProfile_user_2 :: SelfProfile
testObject_SelfProfile_user_2 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "j8-01.8p-a01.1x.itdw"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "", emailDomain = "\SO"}) (Phone {fromPhone = "+80223593805074"})), userDisplayName = Name {fromName = "M\SIa\163741\&5\DLE2w\1078340D\DEL'\140510J\72240wO\144319|k\181929\DC3\1110801%\EM\EM(\1023404\DC1\DLE\FS)\182773ES\180601\178439+FQ4I-\1100841\1095940\SYN\GS\14334\1010976v\140115Z3\1072084d.O~\1009177\SO,\60337\vt\1028177 \983832\1100931\DLEEO\ETB\1072049\STX\1095938"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.SE, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "eg58_94ml"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T10:09:30.813Z")), userTeam = Nothing, userManagedBy = ManagedByScim}}
testObject_SelfProfile_user_3 :: SelfProfile
testObject_SelfProfile_user_3 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "9bp.a5vx8"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "@\1096468Au\127813k\7727\1076864wd\SIJ_bw\110778~\1105251hO\187568\179809\165027\49320\&9\FS\EM\24762>\ACK>,\177486z\ETX\997589\100965\EOT-3h?\1033727\143076\SUB]Nd\988762N;3\1061594B3B\nz\ETX\EOTd\990100_1\1077674I+\72744\1032811\ETB~}\30907^3\1047032U+\ACK\SO\29125JMF\78192{\a#A\SYN\EOT\ESC[n\CAN\DLEc\\"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.FJ, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T12:00:43.178Z")), userTeam = Nothing, userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_4 :: SelfProfile
testObject_SelfProfile_user_4 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))), qDomain = Domain {_domainText = "a34d.j-56"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "\153225", emailDomain = "v"})), userDisplayName = Name {fromName = "\f\1020602e\2079u\144785f{;!B\DC3\142155\141304\t\26345h\1049979\1036768\1095949\49976u\1061371\173737\146411q\37994\RSzc\1029156s}5\ACKp'\n\EOT\182187P\1016086\183869)^)\64520d\13228OIW@L\158153!\1013007>*+\1014878!cL6\1066948e`>\SYNn\RSSeB\1077697W\119899\60339\CANr\984128i\SYN\1040449\1016667\DLE1mX\170323\GS\139573N)<\CAN2 \DC3\SUB\160222%dj\SI%\vP\143011\&6\EMIg]"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.SI, lCountry = Just (Country {fromCountry = CU})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "vom3p_qlmw61wea9ymzq.bqa0lf3wg8t6wv8hxbb85je37p_wag8l_laf1a7yqy9xw6ee8579v6o56-ohdr6l2it4oknmz9t04h6wmj73kwo3o3g9o-54r35s0jd"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T02:53:21.975Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), userManagedBy = ManagedByScim}}
testObject_SelfProfile_user_5 :: SelfProfile
testObject_SelfProfile_user_5 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), qDomain = Domain {_domainText = "aqkwde-j7.m32sxp3"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "n", emailDomain = ""}) (Phone {fromPhone = "+89571249"})), userDisplayName = Name {fromName = "\SI9\41089\1063656\1011581)\169680\&1a\51052\1050847>\1089313\ETX*s*\SUB\EM\ENQX\68454)\fS\988465\DC2 \148559\&9d\99702Mw[q\\\a[Y\DEL\1104178\DC4~\144865Q#\990037\1092231\SI{-2\a+P\5266v"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.OJ, lCountry = Just (Country {fromCountry = PL})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), userHandle = Just (Handle {fromHandle = "e8"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T10:05:17.873Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), userManagedBy = ManagedByScim}}
