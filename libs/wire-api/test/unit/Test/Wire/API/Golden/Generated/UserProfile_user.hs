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
testObject_UserProfile_user_1 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), qDomain = Domain {_domainText = "m1lu3x.mk--p"}}, profileName = Name {fromName = "\993222-@\SO\SYNfW7\ENQ\f'H\167036\DC4\1072645!\11298\GS\92167biMtU1\162039kG"}, profilePict = Pict {fromPict = []}, profileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], profileAccentId = ColourId {fromColourId = -2}, profileDeleted = False, profileService = Nothing, profileHandle = Just (Handle {fromHandle = "tdhnpl_"}), profileLocale = Nothing, profileExpire = Nothing, profileTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), profileEmail = Nothing}
testObject_UserProfile_user_2 :: UserProfile
testObject_UserProfile_user_2 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), qDomain = Domain {_domainText = "7zqy9.emxibs182.7ara8h6.02s.jj"}}, profileName = Name {fromName = "03\STXh#\26149S\USY<\24299\1024717%\EM#9u\f\1080108z\EM\47444e\ENQE}P\1049948\&8kc\165423]\4291\DC3B`Eo\ETX#PZmSPY\146871\r\1032792HubL\1040725\187616fW%\150588q\DELd%\EOT1>5T\GSl\SOH4Mdk\1027302-\239\RS\DELP3\984153\NAK\NAKP+\1052796\1070754\1109926\1042197!8\41733\SOv"}, profilePict = Pict {fromPict = []}, profileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], profileAccentId = ColourId {fromColourId = 0}, profileDeleted = False, profileService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), profileHandle = Just (Handle {fromHandle = "vld"}), profileLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BH, lCountry = Nothing}), profileExpire = Nothing, profileTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), profileEmail = Just (Email {emailLocal = "Ti", emailDomain = "H("})}
testObject_UserProfile_user_3 :: UserProfile
testObject_UserProfile_user_3 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))), qDomain = Domain {_domainText = "w9-b7.h33m.u09"}}, profileName = Name {fromName = "8\DELfO\DC2\FSY\1086863P\1030997{Z\DEL\58879\&9\USh\163075`SzL*rf\DC2\1094497M\ESC\1066817M\43493vj|\1091752^05n\120887AH\SI\a\1074757A\40767\1049140>3\SUB\1104259H\1082489AO\48362\ACK\f\1030446\&33zR"}, profilePict = Pict {fromPict = []}, profileAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete))], profileAccentId = ColourId {fromColourId = 2}, profileDeleted = False, profileService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), profileHandle = Just (Handle {fromHandle = "ydofe"}), profileLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.GL, lCountry = Just (Country {fromCountry = UY})}), profileExpire = Nothing, profileTeam = Nothing, profileEmail = Just (Email {emailLocal = "\984967\1099148", emailDomain = "\1056574"})}
testObject_UserProfile_user_4 :: UserProfile
testObject_UserProfile_user_4 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), qDomain = Domain {_domainText = "k1k5.g8z"}}, profileName = Name {fromName = "R\ETX:mv\99918\174260O7\SIM\23969\&5\1042834.\SOt=\4862x)_0n,\146079\97283Zgl\EM\999511\&9^\1051147\CAN\156838B!\1061924\FS\bM\1017840\RS\r\1104017M\119041\987500N\NAK\1031244\&1\1065986\&5>\NAK3a\15900\184975\1104097#\ETBM`ay4/_GKH\16757\95199\1067733p_*8\ETB\1038177\ESCq.\r:'\145462N\v\vp=\r\11236V>0`#%u\n.<iO"}, profilePict = Pict {fromPict = []}, profileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], profileAccentId = ColourId {fromColourId = -2}, profileDeleted = False, profileService = Nothing, profileHandle = Just (Handle {fromHandle = "up6"}), profileLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KS, lCountry = Just (Country {fromCountry = TR})}), profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T05:27:21.971Z")), profileTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), profileEmail = Just (Email {emailLocal = "", emailDomain = "U$"})}
testObject_UserProfile_user_5 :: UserProfile
testObject_UserProfile_user_5 = UserProfile {profileQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "y4fyb0-4k-xd4.g1n27"}}, profileName = Name {fromName = "-*\162169D\NULTN\1061363\166634\1086epV;i\184252$\ENQ\1083518\1072189PtPR\172089:\v\ENQ\r\ENQ\SYNX\1090801\33326v\1093324r7\1021791)p\FS\30224\ESC\DC2KhN\155857\aXR^], e\1012033\RS!;\SI\83392\164257\1031460\175637Bwr\181047\&82~\1006665gQ`q\ESCuZ"}, profilePict = Pict {fromPict = []}, profileAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview))], profileAccentId = ColourId {fromColourId = -1}, profileDeleted = True, profileService = Nothing, profileHandle = Just (Handle {fromHandle = "ciw5kere0"}), profileLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BN, lCountry = Just (Country {fromCountry = LU})}), profileExpire = Nothing, profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), profileEmail = Just (Email {emailLocal = "\STX", emailDomain = ""})}
