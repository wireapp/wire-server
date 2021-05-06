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
testObject_SelfProfile_user_1 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), qDomain = Domain {_domainText = "4.o"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "", emailDomain = "e"}) (Phone {fromPhone = "+583623261154841"})), userDisplayName = Name {fromName = "q)H\ETXVu\177842\DC2\NUL\fL9`\RS\FSY\1016047\USs\SO\ETBog\\I\DC2\1101292\ACK\984728\1095164-H\1108948}s\136225`\\g\SYN\STX)#Z\EM;N-Qzv\1089352i\1074084/\1002459%43%\1056890@\ETXU<\98675\152923P]{xJ\50534\SYN\72261\183627\46403>\131\1030347B\46926{_\1085107\188768\&8<\113825:\1095174\SOHf!%7\NULi\59645\DC3\DEL}\SI\STX\DEL\DC3\r\994068\ENQ\1106697 H\SUB\SO"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.KA, lCountry = Just (Country {fromCountry = CA})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "vu2fbsz2"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T00:18:57.144Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), userManagedBy = ManagedByWire}}
testObject_SelfProfile_user_2 :: SelfProfile
testObject_SelfProfile_user_2 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "3x775.pb"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "s", emailDomain = "\r"})), userDisplayName = Name {fromName = "\139099\22432\142711\\#f/)\1108752\39070\1017688\1080221#\f-`\DC1\US\ENQ\47325\SO\DC4\167265\133366\DC2]=u\188743#4\DC3f\GSQ$T\95662\&6/\1002050\&8'\993440?R\40084\1036961\GS\rF\1070446`\186368E\1055270mx}\DC1YA}?"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.LO, lCountry = Just (Country {fromCountry = AO})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "znlqlg59eeo.5_cikdu64innkv5y52ukuefkdldo1o_bwm86owfjw9cqr2o6d7typm7nk3u4nr2gnm"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T14:02:57.397Z")), userTeam = Nothing, userManagedBy = ManagedByScim}}
testObject_SelfProfile_user_3 :: SelfProfile
testObject_SelfProfile_user_3 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), qDomain = Domain {_domainText = "f946q.dz1r"}}, userIdentity = Just (SSOIdentity (UserScimExternalId "") (Just (Email {emailLocal = "", emailDomain = ""})) (Just (Phone {fromPhone = "+22212804731"}))), userDisplayName = Name {fromName = "B\48442(x\DC2U\DC4y\1050050SD'\8413]\1048331\1085429O\150455\992772\1091004@\51825p\1100861(\SYN\151602\152225\DELPpP\984020\185170\&8T\1052077:[9,"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = 2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.MN, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T22:12:58.424Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))), userManagedBy = ManagedByScim}}
testObject_SelfProfile_user_4 :: SelfProfile
testObject_SelfProfile_user_4 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), qDomain = Domain {_domainText = "398i5.bvx4u6"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "", emailDomain = "p"})), userDisplayName = Name {fromName = "|\SUB7\SOHA!\RSu ddw\DC2d\39095$8\NUL\1002697\SUB\ESC\SOH\1080251`\1026218\139662$\1105579l\42295\2474f>Hd\169654>,U1\1047268\1003686N$.)\SO\EOT`[\DELQ7(\1018932\&8U4\1032163EM\ESCxD7\1092328*\SUB\DC2\41119K\36291F4bf\DC3P\ETB~\1089573\DLE\189488ad$\SO\1085569\a?\1019802\189444\&4;\1103060kl\ETX"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.IS, lCountry = Nothing}, userService = Nothing, userHandle = Just (Handle {fromHandle = "122i023q_nvmdgsn4qf3lqxfg-vo8-ch4niu5vj8bwmusp8e"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T11:42:19.034Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), userManagedBy = ManagedByScim}}
testObject_SelfProfile_user_5 :: SelfProfile
testObject_SelfProfile_user_5 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "7.z0t68t17-863u"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "^ os6\DLE3\GS\US"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.AR, lCountry = Just (Country {fromCountry = IQ})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "4mk0l6bgzgjqtx_bn4vd40mlww39v.20dphn5je1xkw.oa1r43ftchpzh5gn3dqbo57.l8_ia.5.6q4bnpfmnlh0qe1eo2rwo22h-ucm0edgk6wyk6hskrx_sdyev.j0nakx8iben_ckwz9.scrwju3jfn-6q_x5yytmebed-32jjm4"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T10:26:15.276Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), userManagedBy = ManagedByScim}}
