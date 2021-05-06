{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Email_user where

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
testObject_Email_user_1 :: Email
testObject_Email_user_1 = Email {emailLocal = "\195097\147854\159099]D\ETXV\DC2IW4\1029082\1094475/\DELv E", emailDomain = "&t\15779tet\163033\1035064\1014489\165192(o\ENQ\EOTY\1083116#\17431]\DEL\1105243\140491\48228H`\v\37470\18196X>"}
testObject_Email_user_2 :: Email
testObject_Email_user_2 = Email {emailLocal = "\v\NAK\DLEs ;\41178[", emailDomain = "\1027670$\",\41932\137353yQ%}\168775"}
testObject_Email_user_3 :: Email
testObject_Email_user_3 = Email {emailLocal = "P\STX(U\NUL\DLE \16447\SI\a\US\DLEl\SYN", emailDomain = "\1095086n/wsD"}
testObject_Email_user_4 :: Email
testObject_Email_user_4 = Email {emailLocal = ";n:z\SO\GSA\179129\61881\29625\1105733\1109041+\1031574T{]\EOTsru\1036555w\1002989", emailDomain = "\DC3Dl4"}
testObject_Email_user_5 :: Email
testObject_Email_user_5 = Email {emailLocal = "\NAK=7\997903R2{+Q", emailDomain = ""}
testObject_Email_user_6 :: Email
testObject_Email_user_6 = Email {emailLocal = "\990661^;\170411\21804\173761)\1106887\5310\&3c\DC2=\SO\187537D\1049462\SUBZEBtF\985797\1014506\DC4", emailDomain = "j5\1005191J9&+\6829\12642Z<6\DLE\SYN\SOHG"}
testObject_Email_user_7 :: Email
testObject_Email_user_7 = Email {emailLocal = "-\DC3\168484\179154\29269NpN|x\SI", emailDomain = "E9\646)P]\SUBN"}
testObject_Email_user_8 :: Email
testObject_Email_user_8 = Email {emailLocal = "", emailDomain = "zE\138570\1057118\ESC\RS\984295\1023346<+O[y\ESC\140367i"}
testObject_Email_user_9 :: Email
testObject_Email_user_9 = Email {emailLocal = "w=\28313\ACK\138538-\135264\993135\&4t\CANP{I\t\994467_\37138y\f^\USl[\\Po", emailDomain = "M]?all"}
testObject_Email_user_10 :: Email
testObject_Email_user_10 = Email {emailLocal = "\SO\rmo\1103906\1106721 NH\DC1;/f\a\1029020\1014475\&4b9\169393r\35099\RS+\RS-\SOH\USJj", emailDomain = ".\152508t`7\61167^\1029564\1105599\ACK\\(z\1044265"}
testObject_Email_user_11 :: Email
testObject_Email_user_11 = Email {emailLocal = "brzzp\139452,\38190\ESC\SOH\1102193.R\as\SI&\SI\ESC\142055\EM", emailDomain = "u6#tC<n\ENQ\DC4l\DLE\SOHp\"\1056865'\47037\1059184\1095741\1089553\GS+\30443\1108803\15373\28191"}
testObject_Email_user_12 :: Email
testObject_Email_user_12 = Email {emailLocal = "", emailDomain = "\160211\175106z\152512\r\991280.&\a%2!0U\DC3\SIv \999365\n\1075389\&17\v\NAK"}
testObject_Email_user_13 :: Email
testObject_Email_user_13 = Email {emailLocal = " \74498\1102934\53977\&7\DC3FiN&_\SYNT\1111482:.\NAK\42866\FSs54\22326\174139noq\983963I", emailDomain = "#C\1087032\&3\16868\167073Q)/+\ETB\ESCr!\96110jI4bB\STXQ7\DC2"}
testObject_Email_user_14 :: Email
testObject_Email_user_14 = Email {emailLocal = "d\bQ\1052517S\38031\65617z", emailDomain = "\139135\rd\\\147461q\b"}
testObject_Email_user_15 :: Email
testObject_Email_user_15 = Email {emailLocal = "\171425\43441Z.d\1018185X!\1004437", emailDomain = "gD:NsJ7\b^"}
testObject_Email_user_16 :: Email
testObject_Email_user_16 = Email {emailLocal = "KX\1099720\1048121}o\1076250|\ACK\138211\vG\"\r\CAN\157319p\STX:\n\EOT", emailDomain = "k\DC4\NULxG.\US2\STX\r\1081689yg\ETX"}
testObject_Email_user_17 :: Email
testObject_Email_user_17 = Email {emailLocal = "K=\n)%S=r\1030913\EOTFk\DC1U\1036061\38522\986000.\984237", emailDomain = "\ETX\STX&\"b\ENQ~\1093540"}
testObject_Email_user_18 :: Email
testObject_Email_user_18 = Email {emailLocal = "L", emailDomain = "wE\1094363\ENQF}\1113679\&0}O\32315\1058010"}
testObject_Email_user_19 :: Email
testObject_Email_user_19 = Email {emailLocal = "U}s\1068830p", emailDomain = "\\_bB\DC3Wg\SUB\15461\USz58_\166860\&7\1067616\GS_}sd\1097776\1085517"}
testObject_Email_user_20 :: Email
testObject_Email_user_20 = Email {emailLocal = "[\1020388m\46900\11264k\1056627\1054921u\ESC>,2\DELRg\SOH4U\1024729\DLE\64932>\ESC\1071067", emailDomain = "C"}
