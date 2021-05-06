{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.InvitationRequest_team where

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
import Data.LegalHold
import Wire.API.Conversation.Role
import Wire.API.Event.Team
import Wire.API.Provider.Service
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_InvitationRequest_team_1 :: InvitationRequest
testObject_InvitationRequest_team_1 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.GL, lCountry = Just (Country {fromCountry = CZ})}), irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "\1145\161434\100132v\n}\40627/\1110144\189399W&\SYN6\25379VxU<C>\f\1049929\73093\1080149p\997624\121270\&0\DEL\ACK=\SUBPzb22\RS\99307H\1075429Yi$s\4567\165470\1096044=y\47170\\~C\SOHzO9\125221i\ACK\SI5TF\1083646\DLE\STX\USU\57499\ACK\DC4\NUL\SOH+\DC3!7\RSAF-.\1096903IeJ\SYNU1\n\146729\SOH\EM\DC3\92739UafJ"}), irInviteeEmail = Email {emailLocal = "\49564\STX", emailDomain = "n\STX/\36938\NUL"}, irInviteePhone = Just (Phone {fromPhone = "+00410708"})}
testObject_InvitationRequest_team_2 :: InvitationRequest
testObject_InvitationRequest_team_2 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SE, lCountry = Just (Country {fromCountry = SY})}), irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "2\1001896\STX\77976\1098691[\96963*\STX !RB#\1048981'4\1087917\70109Pa\1025326dh'\65802\USWj\62399\1044498\1027181\&8\vw\DLE~n\186312)\41041*XR\1078408a*V;\a\NUL\DLEKX]"}), irInviteeEmail = Email {emailLocal = "\DC3\t\DC3", emailDomain = ""}, irInviteePhone = Just (Phone {fromPhone = "+79975763"})}
testObject_InvitationRequest_team_3 :: InvitationRequest
testObject_InvitationRequest_team_3 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.ID, lCountry = Just (Country {fromCountry = IN})}), irRole = Just RoleExternalPartner, irInviteeName = Just (Name {fromName = "\tpf6\149946\1071370\985320<\ETX\1094556:\3885Z\144123\13148b\1024987K_\1019973|\r!\142021\SO6\167454\DC4\1106492znRWk\1076315\1097511\SYN\b,s08\23250y'\DC4\17898Gn\1002373\SI\1087143\24265JD\1092309\95098\US`\1105082\25906\\y\1069279\f\95070S\22654\GSt\ACK5\EMp%\1056115(k \111124:\39130l]C0$7\USF\SYN\127028=\1109844\1026200\1056884\1097765[?\1099663\DEL\ESC*\\C<\1078295I&\1009440\DC2\SO(n\b\58495\v\CAN\SOHfr9"}), irInviteeEmail = Email {emailLocal = "_Y", emailDomain = "\994253\47169P\SO"}, irInviteePhone = Just (Phone {fromPhone = "+685734440959340"})}
testObject_InvitationRequest_team_4 :: InvitationRequest
testObject_InvitationRequest_team_4 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.CS, lCountry = Just (Country {fromCountry = RE})}), irRole = Just RoleOwner, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "\1036674\22002R\ft", emailDomain = "B\SOH\RS\NAK"}, irInviteePhone = Just (Phone {fromPhone = "+1094838708826"})}
testObject_InvitationRequest_team_5 :: InvitationRequest
testObject_InvitationRequest_team_5 = InvitationRequest {irLocale = Nothing, irRole = Just RoleExternalPartner, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "", emailDomain = "M\FS;\"<"}, irInviteePhone = Just (Phone {fromPhone = "+4771221890"})}
testObject_InvitationRequest_team_6 :: InvitationRequest
testObject_InvitationRequest_team_6 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BM, lCountry = Just (Country {fromCountry = GR})}), irRole = Just RoleMember, irInviteeName = Just (Name {fromName = "\1001703\SUB\DLE{\SOH'>rBw\n\NAKsOYQU0\RS"}), irInviteeEmail = Email {emailLocal = "\n\DC2\51219\159162", emailDomain = "a\US`LU"}, irInviteePhone = Just (Phone {fromPhone = "+755632034"})}
testObject_InvitationRequest_team_7 :: InvitationRequest
testObject_InvitationRequest_team_7 = InvitationRequest {irLocale = Nothing, irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "Io\SOHB\ETBP\1061720y\1028275\183073iF<O\35508\1019466\65944anRon\35083\987658\EOT\194690o\66832\b\GSP|1\EOT\SI\a\SO\EOT\20398\1035080\98744\171681P\887 6w\ESC\FS\DELlBs\1099431\62377\171978En\1109617\62775\ETB!\163336\182859Z\DC3\65227\DLE\SUB<x*\1083073E\143703]Mc\NULH\159358\SOF7\92338\FS\7013Q\SUBG`%\69640\RSi\\\EM\1052099\ACK9\999998b\DC1:\1085479j5Q\180068\52422\&3W\20822u"}), irInviteeEmail = Email {emailLocal = "=C", emailDomain = "If\US~\1014675"}, irInviteePhone = Just (Phone {fromPhone = "+552044867406999"})}
testObject_InvitationRequest_team_8 :: InvitationRequest
testObject_InvitationRequest_team_8 = InvitationRequest {irLocale = Nothing, irRole = Just RoleMember, irInviteeName = Just (Name {fromName = "\EOTR\119860[g\ACK\74053u`\995155\SYN\STX\1049801\&9*9v\64963\1052823\STX\28989\ESCJ\t:F\1055938\148832\""}), irInviteeEmail = Email {emailLocal = "", emailDomain = "Yt\96596\1098378X"}, irInviteePhone = Just (Phone {fromPhone = "+15890720"})}
testObject_InvitationRequest_team_9 :: InvitationRequest
testObject_InvitationRequest_team_9 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.IO, lCountry = Just (Country {fromCountry = ZW})}), irRole = Nothing, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "N}", emailDomain = ""}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_10 :: InvitationRequest
testObject_InvitationRequest_team_10 = InvitationRequest {irLocale = Nothing, irRole = Just RoleExternalPartner, irInviteeName = Just (Name {fromName = "\3177\NUL\94459%\41018Z\ad\12362\184048Y\167888vw\"@,mF:\100821?\35355b\141332#\182290\19016t\158894\EM,\999862\DC3we$\1099935I\162353\v\31086\185688n#\1076717Od\b\992679\CAN\DLEM/\62821t]\SI,D\ENQ\145571FKF\994793\r46W\145572\1101621\SOH 2\178738\EMu\92917\NAK\1018106\24961\170964\n\">41C\1053686\rR\1064430\ETBm\148996\US\ESC}\EOT'0\138837U\ETB\RSPt"}), irInviteeEmail = Email {emailLocal = "h\a;\184544]9", emailDomain = ""}, irInviteePhone = Just (Phone {fromPhone = "+57454191994"})}
testObject_InvitationRequest_team_11 :: InvitationRequest
testObject_InvitationRequest_team_11 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.DZ, lCountry = Just (Country {fromCountry = RO})}), irRole = Just RoleExternalPartner, irInviteeName = Just (Name {fromName = "q\58722\v7*\RSHH'\1004144,\1113525M\v\41806[5O\f\997024>\33261\168261y\GS\52938e\147982=\1068407\1046510j:\953Kp9Z\1025046j\1033667\122909JO"}), irInviteeEmail = Email {emailLocal = "\1073901TW\"#c", emailDomain = "\128812^xPI"}, irInviteePhone = Just (Phone {fromPhone = "+3784963528927"})}
testObject_InvitationRequest_team_12 :: InvitationRequest
testObject_InvitationRequest_team_12 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.RM, lCountry = Nothing}), irRole = Just RoleExternalPartner, irInviteeName = Just (Name {fromName = "\f\121208\&10p\35920\US\1044370wZ\ESC\DC2`\EOTcO\1070787}\USy\185140Q\SI\DC4AJ\47440\1023470_\EOT/P~\SI\SUB\EM\EOTMw\184594t\bA,\SO\64156\DC2\4026\DC2;\"\SI\1036141\27318\991956-\152134Jlz\63099\FS*T\1092743(,\47843\176264\ACKI8l\1106341\SYN|\1018413a\97292\1069803\1084150s\ENQerUR5;i\NAKAs3z\49967v3\151716\78751\161395\STX\984955\94311o"}), irInviteeEmail = Email {emailLocal = "DtG\NUL", emailDomain = "gC\20179"}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_13 :: InvitationRequest
testObject_InvitationRequest_team_13 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.WO, lCountry = Just (Country {fromCountry = SK})}), irRole = Just RoleMember, irInviteeName = Just (Name {fromName = "\NAKE\US.{L]C]O\62958Ki\179807"}), irInviteeEmail = Email {emailLocal = "ax", emailDomain = "\1094627$]"}, irInviteePhone = Just (Phone {fromPhone = "+2949280329"})}
testObject_InvitationRequest_team_14 :: InvitationRequest
testObject_InvitationRequest_team_14 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.VE, lCountry = Just (Country {fromCountry = LY})}), irRole = Nothing, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "\1106781\DC47", emailDomain = "m\43655\a\DC12"}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_15 :: InvitationRequest
testObject_InvitationRequest_team_15 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KJ, lCountry = Nothing}), irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "\b\1015486_\r9#,x\170582\1110388P*\50591~\SUB\10449b\999487f1\120738\SOH\32277V\64085{\986863Xg\SOHT\1091551\RS\110701;mT"}), irInviteeEmail = Email {emailLocal = "n7(\ETXRg", emailDomain = "t"}, irInviteePhone = Just (Phone {fromPhone = "+3485090439111"})}
testObject_InvitationRequest_team_16 :: InvitationRequest
testObject_InvitationRequest_team_16 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.AV, lCountry = Nothing}), irRole = Just RoleMember, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "IL", emailDomain = "I9w\1042488\1045922"}, irInviteePhone = Just (Phone {fromPhone = "+50003509553367"})}
testObject_InvitationRequest_team_17 :: InvitationRequest
testObject_InvitationRequest_team_17 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KS, lCountry = Just (Country {fromCountry = BL})}), irRole = Nothing, irInviteeName = Just (Name {fromName = "\1075168{\DC4N\NAK~\SUB\CAN\1043873\&0\EOTpf\150735\1056001\993681\995049b~~\988179\a"}), irInviteeEmail = Email {emailLocal = "4\155853h]", emailDomain = "p,\1007103"}, irInviteePhone = Just (Phone {fromPhone = "+7013336911"})}
testObject_InvitationRequest_team_18 :: InvitationRequest
testObject_InvitationRequest_team_18 = InvitationRequest {irLocale = Nothing, irRole = Nothing, irInviteeName = Just (Name {fromName = "\txM\190783\RSP\NUL/A\EOT2.VK:Q/\12369\&4\144654/1e`t\b&Q\1062600\&5z\CANb\1007464\akj\1010024V\22902U Ub,@\64396e1\1111717l9,\STX\1099029\1098246"}), irInviteeEmail = Email {emailLocal = "\bn\FS", emailDomain = "2"}, irInviteePhone = Just (Phone {fromPhone = "+6997822608835"})}
testObject_InvitationRequest_team_19 :: InvitationRequest
testObject_InvitationRequest_team_19 = InvitationRequest {irLocale = Nothing, irRole = Nothing, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "", emailDomain = "!\1055687\66653"}, irInviteePhone = Just (Phone {fromPhone = "+7329000328"})}
testObject_InvitationRequest_team_20 :: InvitationRequest
testObject_InvitationRequest_team_20 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SN, lCountry = Nothing}), irRole = Just RoleExternalPartner, irInviteeName = Just (Name {fromName = "^\61737Z\97152L4on4`M.\ESCxU{9A\ESC\ETX2\STX:la2F\USU\NULk\147190W~\EOTi\72417\"J:m"}), irInviteeEmail = Email {emailLocal = "\32012", emailDomain = ""}, irInviteePhone = Nothing}
