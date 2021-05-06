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
testObject_User_user_1 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "x6.y0f"}}, userIdentity = Just (SSOIdentity (UserSSOId "" "") (Just (Email {emailLocal = "", emailDomain = ""})) (Just (Phone {fromPhone = "+014178179199"}))), userDisplayName = Name {fromName = "lr1={\NAK\SOG7K\ACK\\]\51775\NAK5\991969\62802\ETB\1066082S\121315\GS\DC4\1047224\2235\1017997w"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.NL, lCountry = Nothing}, userService = Nothing, userHandle = Just (Handle {fromHandle = "hh5ppevh"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T05:14:24.111Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), userManagedBy = ManagedByWire}
testObject_User_user_2 :: User
testObject_User_user_2 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), qDomain = Domain {_domainText = "r1z8m-6o.g"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "", emailDomain = "e"})), userDisplayName = Name {fromName = "\FSp@\151805q\986316\&6\10365_)B#)VQ\SOH\ENQ\27438JioQ\SYN\SO"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.KY, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), userHandle = Nothing, userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), userManagedBy = ManagedByScim}
testObject_User_user_3 :: User
testObject_User_user_3 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), qDomain = Domain {_domainText = "tt-h3d8vh12.x1"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "n", emailDomain = ""})), userDisplayName = Name {fromName = "Q\40301\&1G\\\57410\162373g\78766\CAN\49380\984264\EOT\DC2V\SI~\EM\EOTb\SYNA\SOH"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.SA, lCountry = Just (Country {fromCountry = SX})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "a6rlhe5r4p0ugb915o05-cs07fchsz5fs6lqujlu0ax86bi1xqy4o37j9d-u6hw89vq__3nml5dv6z3bmier-q86yeq3_n32k9f2zgc5x.qznxhyeetfvfb0z26lkaxtsajmh8-ennn9h6qr9n745pg4p.n._3u-wo.-x7k"}), userExpire = Nothing, userTeam = Nothing, userManagedBy = ManagedByWire}
testObject_User_user_4 :: User
testObject_User_user_4 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), qDomain = Domain {_domainText = "x1.m9qo.gl0azd6b53.vsg"}}, userIdentity = Just (SSOIdentity (UserScimExternalId "") Nothing (Just (Phone {fromPhone = "+171510085209"}))), userDisplayName = Name {fromName = ")M\SO\fBQm[\98831\SUB\1031386\1007162y&\991399\54209\&2\40648\STX\CANr\174224\1043795\SYN\1037244\&3W\50037:\128257TSB(\ACKVb\1097734;$\a\DC3<\62391\74521\1097339\61409\1113878\1089669*y\CANd\1104352]ov\1108850~\38998@K\181547U\140764-\1075632%>\DELA\ETX\1055275e\DEL/7\NAKx\bq\DC2x9m~-\154468</$'\t\1019068v_,\39149}\SI~\161626wX:\DC4\CAN\FS$\ESC\1075983\1058919\SOH\DC3\ENQ"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.OM, lCountry = Just (Country {fromCountry = RO})}, userService = Nothing, userHandle = Just (Handle {fromHandle = "45r56fpxg2nddux0rw.msn15u251_faei51zue42ou4xg0dljir0wo5kdxharsizb_g4r7xchhxloxidsc1setj1zp.romfhtqsyu8e5hdqe.j75xfzx"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T15:20:22.077Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), userManagedBy = ManagedByScim}
testObject_User_user_5 :: User
testObject_User_user_5 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), qDomain = Domain {_domainText = "f9-5sx0gj1u.ya"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "@p\1066599po/Zx\1048983zR&frExG\1097738ok3M\152177R+WLo\SYN\fB\1018959\ETB\190071o|\1005189\&3\1058221\\:\7794\DC3\1012519ik0G\43478\1059379\1110186\158862o48\187444R\1093586\83063W\1000526j\1085092(\STX\a\1081224\&0L\r7 \1051999-Q9\32488\&6\142903\1013524Y \157105X \168118\128211/G\1054718\STX\1008031drS\ACK=\174373h\NAK\1021628"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "\14658" (Nothing))], userAccentId = ColourId {fromColourId = -2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.VI, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), userHandle = Just (Handle {fromHandle = "sih09lnnppl_sl"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T21:25:54.248Z")), userTeam = Nothing, userManagedBy = ManagedByWire}
