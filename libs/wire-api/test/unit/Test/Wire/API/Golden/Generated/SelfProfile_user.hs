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
testObject_SelfProfile_user_1 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), qDomain = Domain {_domainText = "2-qfj78.7--h-o-2.cg55"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "n\GS\1016438:\1061191\1044918\173190\NUL'j\1010727\v5WA%\DC1\1031443\992434\1005678-\DC14\FS\SO\SI\176808J*G\1077730\SYNA\SO\52516(\1062473\SO-H\b"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.CH, lCountry = Just (Country {fromCountry = BW})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "vgyym72wn7uo6fe3xb897ptkna6w-4ow.npftrbfnn5uc2tzljys12dj_nb5cqbz-mrnazq65"}), userExpire = Nothing, userTeam = Nothing, userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_2 :: SelfProfile
testObject_SelfProfile_user_2 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), qDomain = Domain {_domainText = "m95-i5s76c0e1.v"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "", emailDomain = ""}) (Phone {fromPhone = "+23483009"})), userDisplayName = Name {fromName = ";\EOT\180152&[I\186956\1051600\168854X\NAK\54243b]\1063273\&0P|j\917561!\CANf\39606\119045\1064720\1091995\33426\988192oB\SYN\64204]\1002846eO\EMP9r\1084294\&6v\177316\20599B*\SIg\48788\&5x\1083828&\FS N\DEL,R$\SI"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.HU, lCountry = Just (Country {fromCountry = BL})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), userHandle = Nothing, userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_3 :: SelfProfile
testObject_SelfProfile_user_3 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), qDomain = Domain {_domainText = "zj.sr-d"}}, userIdentity = Just (SSOIdentity (UserSSOId "" "") (Just (Email {emailLocal = "", emailDomain = ""})) (Just (Phone {fromPhone = "+4989256048"}))), userDisplayName = Name {fromName = "K9\NUL\DLELU\1032592\&7\US\CAN_xvO=P4\996290z\DEL\a\35084|\NAK\1081417b\SO@\23398\49066_\171339p\ETBe5\1111711\r\1107643,\n\rueziL:#\CAN25*\1111895\1110934}\1051447\DC4\1078196\ETBI\r\ESC\985937\ETX\36685\&5B9\1063179rSH}w+BR6>h|\1001195N\n\EM\1035860\ESCb\30719SK\174254\NAK\69993H\US\10792\DC2F\v\vb\ACKEM"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.IO, lCountry = Just (Country {fromCountry = IE})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T03:47:28.501Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_4 :: SelfProfile
testObject_SelfProfile_user_4 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "a-m.r7h"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "", emailDomain = "U"})), userDisplayName = Name {fromName = "F\tFpc\SUBg\STX\ACK2\DC2\r$\t\EOT\"}]`\GS\146074\&8-B=\b?\1036218\SUBJ\DC2}b2\28022\a\ENQ\1087043\136518\54668\1072995$E:\SOH\156969\1039353B\1084228\1065189GTl4,TY.Cd*;\19719\1082496&|`\1030014\DC1J^*W a\n\f\1103622\4206*2t-Z\DC4h3\34343"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.DV, lCountry = Just (Country {fromCountry = SO})}, userService = Nothing, userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T02:00:19.762Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_5 :: SelfProfile
testObject_SelfProfile_user_5 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), qDomain = Domain {_domainText = "059a.u3--1---oj-3"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "\1091531\NULb\19677\SO\EOTCg3\1039912\1027455}\180801k\1109451\DC4\1038377\917959\RSm[\b\RS!G6#hJ\DC2g\US\147960.?\1108650agvephsU5K\STX-\aKj1\1071617\aP. &\EOTo\1003558C0<y\156758\r~2\1081187\n\149334\167158<5>O8Go@9\DC3o\58599BR\SItyCZda\5811\&80\1052352\&4\1057641m\1082117^1'\tev\SUB\1077693s\\O"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = 1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.MG, lCountry = Just (Country {fromCountry = PH})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "lt"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T05:33:35.871Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), userManagedBy = ManagedByScim}}
testObject_SelfProfile_user_6 :: SelfProfile
testObject_SelfProfile_user_6 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), qDomain = Domain {_domainText = "0e81j04g5k6.g7526p0--7t"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+2000888600437"})), userDisplayName = Name {fromName = "\1024109H9<{\181107\&0@N\v'LD"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.OR, lCountry = Just (Country {fromCountry = SY})}, userService = Nothing, userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T10:59:59.088Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_7 :: SelfProfile
testObject_SelfProfile_user_7 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))), qDomain = Domain {_domainText = "ek.2.v-9i5"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+103027942287148"})), userDisplayName = Name {fromName = "R\SI}\1095518N\9604\176893\1059561\b\93046s$|\1028512'>,\"\SO\SO\&HkT6\1029630\t\1104244\1064758e:\51617\EM\1022172\138766\991875wU4%\\z\DC3\CAN\DC1\GSe\32433\1012603`k\CANA\SUB\1051797\ENQ\n\4753\1021933\1101045CN:\180050]\1081350\10177\181008\\8\r\1070330&(Fr\97159Ng\NAK\1034526]e\aLdK\DC3\GS\160718'a\1013868e9\32160\1095376)7\1046520\DC2NLD4\SI\a\SI\SUB"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.CR, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "m9q4613.3owi-b-km02q54kfkst.c-znmnbxutw_46c.1f.k_olbc5gwkvoydw88yaj8_9u5u0568qlyas.79u4vbgteqfit_yzaa55wf-7kam7vi_"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T03:01:02.116Z")), userTeam = Nothing, userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_8 :: SelfProfile
testObject_SelfProfile_user_8 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), qDomain = Domain {_domainText = "isg.a"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "", emailDomain = "X"})), userDisplayName = Name {fromName = "B2&\EOT[78w\161787Paz \ESC\182871\NUL\1017442\&6E \NAK\b\DEL\1105086\EM\176222\&2A,w\1007589\vA\65322\149421\120677"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.KV, lCountry = Just (Country {fromCountry = GG})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "m8ebp32yd87n-7yb29t930qea8b2f63ici8qlmjg3cjnelzmbcailm.i4f7x7h19r5j3e7ntqx7j825_gh7-5jdrs5kyjgdnu4dfn1q6ppwivp5jrt-27qhpvxm48om0v7cs8ya022chge1hloe139g9725k.h521-javeglu4s"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T18:31:33.442Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_9 :: SelfProfile
testObject_SelfProfile_user_9 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))), qDomain = Domain {_domainText = "m.3.553.xdd6e00z.s8.i"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "\DEL", emailDomain = ""}) (Phone {fromPhone = "+30090801530194"})), userDisplayName = Name {fromName = "su2\132211Y\RS+\US\1086099m\187957\ETX\DLE\DC2WmhR\1052548p\FS\EM\SUB\993821\163531o\EOTM^6gn\DC2\STX@\1098987 \SUB\1010239DO#\SO\"\DELyG--&\\z\NAKhp"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.OS, lCountry = Just (Country {fromCountry = EC})}, userService = Nothing, userHandle = Just (Handle {fromHandle = "51p"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T17:03:25.883Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_10 :: SelfProfile
testObject_SelfProfile_user_10 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "944--bbifn16f89t.eui347e1o8c3lj"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "", emailDomain = "z"})), userDisplayName = Name {fromName = "[Npz\fdPr\r}\61931\51687\10027\ajb\97985\t\v\ETX\1011600\GS\1101909B'Mw=&2w\51007x\SOH\ETBH\ESCM!{i=l\ENQ\vMH!}*"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.MY, lCountry = Just (Country {fromCountry = GB})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "s7phg"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T08:38:03.268Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), userManagedBy = ManagedByScim}}
testObject_SelfProfile_user_11 :: SelfProfile
testObject_SelfProfile_user_11 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), qDomain = Domain {_domainText = "durgnm8kp.xo56.kd-30k"}}, userIdentity = Just (SSOIdentity (UserSSOId "" "") (Just (Email {emailLocal = "", emailDomain = ""})) Nothing), userDisplayName = Name {fromName = "\67682l8N\r\EMZ/KV\50812\65761L/\DEL4\126568JxP'\b\986483"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = -1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.HZ, lCountry = Just (Country {fromCountry = SD})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T22:13:58.865Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), userManagedBy = ManagedByScim}}
testObject_SelfProfile_user_12 :: SelfProfile
testObject_SelfProfile_user_12 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), qDomain = Domain {_domainText = "m.d-0"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "i\US\998199"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.FA, lCountry = Just (Country {fromCountry = FI})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "9usrti4"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T10:21:43.138Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_13 :: SelfProfile
testObject_SelfProfile_user_13 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), qDomain = Domain {_domainText = "oae3.x-6"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "\1021317", emailDomain = "\1049086"})), userDisplayName = Name {fromName = "\73805+=?o\FSd2\SIPJ\t\162508\SOE\133465'\rB&\1062482SHP4g\SOH\157599\DEL\SOHq\1034471aC]T;I(,\168124\1027275\151209HS13\ENQ\GSdT\54083\ETB\\\1093191\NUL\t\69693Y3=e\STX\29224n&K\DC3\72996L\EOTUO\28419\SUB\EM1]\1016458\nBWvo\7021\20318\18629Os\tI1.\EOT\1038312t^\121512\25219AYp\rjJ[@"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = 1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.TT, lCountry = Just (Country {fromCountry = NR})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "-qxv9chtzyfcl04j3vzry1g-in74-evmd.--qxq5f_ze3j0j-3jglq8u6vddnrg1wo51si-yp0lw5cf315ap55pse5k-igz_vi5b-58ccxip7yk_s3z2-4tqd1fqokcvutd9ngw.0xsmn.bm.n0pto.ieldgqxomm0abmql7b89h_91ndm61"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T08:01:01.571Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_14 :: SelfProfile
testObject_SelfProfile_user_14 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), qDomain = Domain {_domainText = "2g7-g.i--vv83.c5lkki1np.hh8-3-gl6.m12.7f9-07.v5a3r"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+2096518137"})), userDisplayName = Name {fromName = ")\DELDu\1084049Ay\nFT\1060036&l\"\134222h0\58950Q(=(1v\174296\&2\DC3\STX\tI`\36957\EM1j\\\NAK\EOTbtq2'uu,\987397\EM\ETBjrP\28204\SO\1003462\ETB{\\\138893\DEL\47142ghD\DC2b:SR\"|&&:u\83321XatF}IO\v\148276\189877\b(rE=\ESCQ\61737\132382xf\986099*\1024310$RR\RS(rr\3966w\168939p8rT\DC3\1113188:;]\99692\&8("}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = 1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.AZ, lCountry = Just (Country {fromCountry = AO})}, userService = Nothing, userHandle = Just (Handle {fromHandle = "ggm3-8wtq0h2hmo94u4cq94_-gz-zsmthwss5u5pftjjku.ulp83oz6db9a_okejkzjylo98aavgrrg5xa32m7jv.hlo_fd_u80_sfjc7dso___6kmef_fxuaykbzfpk.enk349q491u6bkdvpxpd-0nvk"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T13:36:06.246Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), userManagedBy = ManagedByScim}}
testObject_SelfProfile_user_15 :: SelfProfile
testObject_SelfProfile_user_15 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), qDomain = Domain {_domainText = "m-c55.o05"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "", emailDomain = "\v"}) (Phone {fromPhone = "+901064836280188"})), userDisplayName = Name {fromName = "\5651\a\136222\GS\SYN\ESC_\29384&(\ETXhUE\t\CAN&|=zj&\EOTYA\DC3\1082642\&6\61777F\27633)(&!`^\72230\127399\99170Sz\155998\160355\1067684\49681d O\989045(\155609\1074117_\fg[#\1081589RNw\1073788/QA"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = 1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.FR, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "g9"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T21:08:26.830Z")), userTeam = Nothing, userManagedBy = ManagedByScim}}
testObject_SelfProfile_user_16 :: SelfProfile
testObject_SelfProfile_user_16 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), qDomain = Domain {_domainText = "6138.3--919.q29t"}}, userIdentity = Just (SSOIdentity (UserSSOId "" "") Nothing (Just (Phone {fromPhone = "+0853144127"}))), userDisplayName = Name {fromName = "+\ETXl\51131\1015739#M!\62956I"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.NA, lCountry = Just (Country {fromCountry = GR})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "jzyrzdl38-sofb205djp.6-l5998rfn0nzbsh6w3yr6rhp7.fne6uqoa31h_x6j2931wjzsr1fkschx8.jwmsb.o.69gog"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T01:21:13.795Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))), userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_17 :: SelfProfile
testObject_SelfProfile_user_17 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "52x98.q7"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+26816549404310"})), userDisplayName = Name {fromName = "\f)*\DC2\DC4\DC2\f\GS\1016357I\39992R\DLE\167259>k\\FZ\EOTX|q\NAKtP=\ENQ8#:\a\FS\186204Lo<=\1038949\&5\SUB\1064778FZ\49384"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "\173466" (Nothing))], userAccentId = ColourId {fromColourId = 1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.IA, lCountry = Just (Country {fromCountry = SH})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "c.1zyr_ml6p.a6s74xp6703l.52cbda-6wm-1q.53hjmokyyt9_lp2ruaajgu0.yw01i_2_vscfr.k18_6bxl6qz7rtszhzejslu94i02fsfx7g.pusr5cw_qn.awtq_h8457zoquz2luhb.eh6irfshc-qwkthkxaxb7csodmxhy6akaa8_ehzar"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T14:52:39.663Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_18 :: SelfProfile
testObject_SelfProfile_user_18 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))), qDomain = Domain {_domainText = "8rf.3.19bzrr90a3.lwx"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "\DC3", emailDomain = ""}) (Phone {fromPhone = "+80474100330"})), userDisplayName = Name {fromName = ".3R"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.XH, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T08:18:41.407Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_19 :: SelfProfile
testObject_SelfProfile_user_19 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), qDomain = Domain {_domainText = "pez9w8x-j9kj8b6.a-q"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "", emailDomain = "\1033337"}) (Phone {fromPhone = "+60013250"})), userDisplayName = Name {fromName = "h{@\t\62622+\ETX#%\SI:;\a\126509\"\63666["}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = 1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.CE, lCountry = Just (Country {fromCountry = DE})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "y6z7aj_c6_sby.5pj"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T14:32:49.883Z")), userTeam = Nothing, userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_20 :: SelfProfile
testObject_SelfProfile_user_20 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), qDomain = Domain {_domainText = "nh3e1-k.5l91.63h58.l85.cxs1"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "<`\1046624\DC2\64382GW,_\ao CR\ESC/Bd\CAN\SYN\995250\r\ACKY\STXl\EOT\1041812\1074282\DC2u0\DC1\ETX\189751x\NULP\1037694`\f\134623{\61176\1092752\1103345\&2\DC4\a\SOHK:\EOT\999589i$jp\1084164k&\1101559r\"vgT\t\NUL\NUL Qv'\nF;)\DLE:P\989935\DC2\EM}\ENQ\147578\1055074\194825\1080040:JB\t\129138<\1038315\169077\988469H\168265\RS\DC3\DC1:\SID\DLEcS\aN7\23772"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.SR, lCountry = Just (Country {fromCountry = AM})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "3e"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T20:30:39.820Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))), userManagedBy = ManagedByWire}}
