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
testObject_UserProfile_1 :: UserProfile
testObject_UserProfile_1 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "0j.z.0l-y275.h--1f3o"}}, profileName = Name {fromName = "\151855\1089179q\ESC\DC2.m\EOTf\t]\171751\DC4\NAKt\127188\DLEI\1099086z_s\SI\1050817\FSs%x\1049712\f^f\1016876Wn\ESC#<l\95187l1g\985234To\EOT\NAK;\DLEE\993247\145586N\va\"l\DC40\1026513t0\RS\996693}\rfr\1089751[\EOT\45547gDa~\30402*\179742P \23025%\SOn\1073880\1102752qF!\RS\aW{"}, profilePict = Pict {fromPict = []}, profileAssets = [], profileAccentId = ColourId {fromColourId = -1}, profileDeleted = False, profileService = Nothing, profileHandle = Just (Handle {fromHandle = "7uqvlhyd_kxap-t70k71z7.vr4q_bkh_7uphlaabn5t30u5hcgykufxcgs-3hiq9.2or09lhp99j2.rwfh05e.0tt09dupb3.gyj7bo.w067kheza5f6ppelf611tgbrxh761kn.te"}), profileLocale = Nothing, profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T21:40:40.481Z")), profileTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), profileEmail = Just (Email {emailLocal = "", emailDomain = "SE"})}
testObject_UserProfile_2 :: UserProfile
testObject_UserProfile_2 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), qDomain = Domain {_domainText = "yoqu-6.t162m8jj52x4--z7y"}}, profileName = Name {fromName = "hd\34685\159052$\ESC#C\98051\US;F\US\990982:\"\151608r\SYN"}, profilePict = Pict {fromPict = []}, profileAssets = [], profileAccentId = ColourId {fromColourId = -2}, profileDeleted = False, profileService = Nothing, profileHandle = Just (Handle {fromHandle = "de4oha2lo.x6nhtz1t7nzbzeosh0o2mt4g_ovkz7g2e3p8evzsqe4pozfoead3rhr5wlpgziv2oc8lmt9sx8iwmqppv.slo2a."}), profileLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.AE, lCountry = Just (Country {fromCountry = TZ})}), profileExpire = Nothing, profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), profileEmail = Just (Email {emailLocal = "", emailDomain = ""})}
testObject_UserProfile_3 :: UserProfile
testObject_UserProfile_3 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))), qDomain = Domain {_domainText = "2.34k00.z.m2-t26-51632r5-z"}}, profileName = Name {fromName = "\64735]OV\DLE?&\1047163\1023598`(e\1112564\GS\ETX~G \37257\ETX\ENQ\8157$ls\1041519E5)1\1040470J4.\NUL[\ETXnu\b\1029031_\64814qbb|wEa\r\1072108m?\DLE\CAN}\6627j:5\133436@\f3\CAN/[;\n8\1094864\n){>\DC1\DC4\1035791I>\989432TDQ\42853Gj\190299"}, profilePict = Pict {fromPict = []}, profileAssets = [], profileAccentId = ColourId {fromColourId = -2}, profileDeleted = False, profileService = Nothing, profileHandle = Just (Handle {fromHandle = "5v21ycovn_bt8gasirepwy7yti3ch"}), profileLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.NV, lCountry = Nothing}), profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T19:05:46.479Z")), profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), profileEmail = Just (Email {emailLocal = "j", emailDomain = "p2"})}
testObject_UserProfile_4 :: UserProfile
testObject_UserProfile_4 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), qDomain = Domain {_domainText = "n.te3-9d8ql4420"}}, profileName = Name {fromName = "X\FS)\GS\SOu\NAK\DC1t`NjX\1057975$\SOa\72245/\176239\DC4\1105930zi|EVH] q\999550h\STX\1067127U\191196J\1069259s\146604j\US+4H\1023152\GS7!n *\1032726\NAK\EOTN"}, profilePict = Pict {fromPict = []}, profileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing))], profileAccentId = ColourId {fromColourId = 0}, profileDeleted = False, profileService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), profileHandle = Nothing, profileLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.IO, lCountry = Just (Country {fromCountry = PK})}), profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T06:12:13.351Z")), profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), profileEmail = Just (Email {emailLocal = "", emailDomain = "J\ACK"})}
testObject_UserProfile_5 :: UserProfile
testObject_UserProfile_5 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), qDomain = Domain {_domainText = "461.p-b970rzl"}}, profileName = Name {fromName = "\165763\1091160\SOH-\STX>\FS0LI;\41914\137114M 8\"\1032848\31957\ESC\1055692\&5J$qp\USk\1077819\19035\&9w\GS\"\129578`\ACK=\38346Z4\1073105\984090L\rT4&\CANu%Fe\DC2d\1031755V\22021\1023634\to\1011152\155824\1182tY'An\12397Y\DC3\USCD&\53907C[\159384\\9+\59383\\\42149X\39071\GS\1090238\1111880\&4[\DLEd`f\11425o\35973\1054752\ESC\20638f\GS"}, profilePict = Pict {fromPict = []}, profileAssets = [], profileAccentId = ColourId {fromColourId = 2}, profileDeleted = False, profileService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), profileHandle = Nothing, profileLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.PT, lCountry = Nothing}), profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T23:21:02.099Z")), profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), profileEmail = Just (Email {emailLocal = "\1109722", emailDomain = "\121256"})}
