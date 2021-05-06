{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.EmailUpdate_user where

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
testObject_EmailUpdate_user_1 :: EmailUpdate
testObject_EmailUpdate_user_1 = EmailUpdate {euEmail = Email {emailLocal = "\1035253R\44032\DC1B}\1962\DEL\1092271BS\"\1034990\EOT\NULu\DEL\1088625\1021450\98348\SYN\991098LqN\1042372T\EOT\ETXQ", emailDomain = "Z\1008077X\37160\1028327/.~\ESC%;{\1045579&k\DC3\1054498{\nQWH\ENQ"}}
testObject_EmailUpdate_user_2 :: EmailUpdate
testObject_EmailUpdate_user_2 = EmailUpdate {euEmail = Email {emailLocal = "v?a\153191g\n\NAKtTG/", emailDomain = "9\FSJL[\SO\tEK%\1101241\1034948\1003065Ol"}}
testObject_EmailUpdate_user_3 :: EmailUpdate
testObject_EmailUpdate_user_3 = EmailUpdate {euEmail = Email {emailLocal = "\v\1091189E", emailDomain = "\DC3bn"}}
testObject_EmailUpdate_user_4 :: EmailUpdate
testObject_EmailUpdate_user_4 = EmailUpdate {euEmail = Email {emailLocal = "\156862NS6aVEp\1075618/\1109186v\1035017\DC4\33400=8\ENQGhz\SYN\74640<\65910Z", emailDomain = "FkN\1058726\DC1 S>5]e5\171291&\176453\160024i\125255\120987 ?\1103504\144413\1075659bL\US"}}
testObject_EmailUpdate_user_5 :: EmailUpdate
testObject_EmailUpdate_user_5 = EmailUpdate {euEmail = Email {emailLocal = "", emailDomain = "\EOT"}}
testObject_EmailUpdate_user_6 :: EmailUpdate
testObject_EmailUpdate_user_6 = EmailUpdate {euEmail = Email {emailLocal = "v;[|\ACK\100898\&3\169466\1080830Ri\DEL\STX?\1092666", emailDomain = "bH\1070225j46\177930\182301qc.\166450\17401"}}
testObject_EmailUpdate_user_7 :: EmailUpdate
testObject_EmailUpdate_user_7 = EmailUpdate {euEmail = Email {emailLocal = "^_Xv\1094352\&3]LZ(\ETX\a2\164936\989086,\">SS\1066024", emailDomain = "\118934j\995032)R\STXgvb#fh>\23555\&5y\300\39258r\38045LA"}}
testObject_EmailUpdate_user_8 :: EmailUpdate
testObject_EmailUpdate_user_8 = EmailUpdate {euEmail = Email {emailLocal = "\1060723X|\b\148364E\1067101", emailDomain = ""}}
testObject_EmailUpdate_user_9 :: EmailUpdate
testObject_EmailUpdate_user_9 = EmailUpdate {euEmail = Email {emailLocal = "Znl\"\EM1`M\57460\b\NUL1\1023115l\DEL\1038926J2\1087984j\1088447/R", emailDomain = "O\48538wr#\158245+\187337nh_\US\988085\f}\USd\DC3|2k\DC1"}}
testObject_EmailUpdate_user_10 :: EmailUpdate
testObject_EmailUpdate_user_10 = EmailUpdate {euEmail = Email {emailLocal = "\170830r8\1100575kf\DEL\158913tY", emailDomain = "\132510\CAN<0w\985099\STX}Y\155248\133745G\78586\aX\68482kt\f\1062883tvfD\30811"}}
testObject_EmailUpdate_user_11 :: EmailUpdate
testObject_EmailUpdate_user_11 = EmailUpdate {euEmail = Email {emailLocal = "hwY\DC3l\ai\168295X\1067963!\1102987\ESC\23803", emailDomain = "\DLE|\SO\73984C\EMt5LA\1021479[L \25801Y\10466<\1015721\&3\14244~\FSL\DC4"}}
testObject_EmailUpdate_user_12 :: EmailUpdate
testObject_EmailUpdate_user_12 = EmailUpdate {euEmail = Email {emailLocal = "\NUL\DC4>G u", emailDomain = "\CANf\STX,(\63472X\65257\&2\f\764pR\165674\1021078B2\28943-\1080473\986815\ETB\1108890"}}
testObject_EmailUpdate_user_13 :: EmailUpdate
testObject_EmailUpdate_user_13 = EmailUpdate {euEmail = Email {emailLocal = ". \ETX4\1085702\EM\1102963$\997583x\183204\191255[.\DELi\DELTa<|\ENQ\CAN\60320\&5\ESCy\ACK", emailDomain = "\49821pP\RS3>|^A4\n\144425\99241\CAN|-c"}}
testObject_EmailUpdate_user_14 :: EmailUpdate
testObject_EmailUpdate_user_14 = EmailUpdate {euEmail = Email {emailLocal = "Y?r]Ka\6021V\a\1086821A8+T{", emailDomain = "\1020768\1069158'z#YH\STXu\1094192w\n\DC4\183233k/+|\1076660\SO\EM\ACK\19781\&9Q cC\24584"}}
testObject_EmailUpdate_user_15 :: EmailUpdate
testObject_EmailUpdate_user_15 = EmailUpdate {euEmail = Email {emailLocal = "\44541p\NAK\1064628\CAN\FS", emailDomain = "EOw_u,\GSH\1046255lE\SOH\\\509\111290?\1100237lvO\SOHt:\DLE\991842\1070257V%\f"}}
testObject_EmailUpdate_user_16 :: EmailUpdate
testObject_EmailUpdate_user_16 = EmailUpdate {euEmail = Email {emailLocal = "\127472\DC2\DLE)Z>r\185262\54039\r2r2\SO0\182834\\\1051523n\1072053\1041049\\XaI\t\CAN\135363", emailDomain = "j"}}
testObject_EmailUpdate_user_17 :: EmailUpdate
testObject_EmailUpdate_user_17 = EmailUpdate {euEmail = Email {emailLocal = "1?!HV\DELo,^\1073686\1022773}m'>\1097683", emailDomain = "\nt9\1084363\92170\SOH\1095782>/qw<\998804\NAKoT:\58498bm\n\128362\10072"}}
testObject_EmailUpdate_user_18 :: EmailUpdate
testObject_EmailUpdate_user_18 = EmailUpdate {euEmail = Email {emailLocal = "\54403>W\SYN\GS\1035015\51954\&4u$ Ji%,je-d\983057\1016655\23228L\45410", emailDomain = "T"}}
testObject_EmailUpdate_user_19 :: EmailUpdate
testObject_EmailUpdate_user_19 = EmailUpdate {euEmail = Email {emailLocal = "M[\SYN+yNjq\SO9?2\DC2!kBh", emailDomain = "T\36722:\SUB\1023530}\GS_0syn\DC3D{"}}
testObject_EmailUpdate_user_20 :: EmailUpdate
testObject_EmailUpdate_user_20 = EmailUpdate {euEmail = Email {emailLocal = "\ACK\\\NAK\50154H\1039284\12316w\173356MZ\164449\58200\1041596\58534b^\1092842&k", emailDomain = ")|\1097962)C\1047037\f\1022236\SUB[ Sl\162101\US'\DC2\b\148476\1012656`\144133ih"}}
