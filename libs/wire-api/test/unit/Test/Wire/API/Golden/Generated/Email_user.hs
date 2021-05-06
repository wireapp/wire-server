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
testObject_Email_user_1 = Email {emailLocal = "Uf\EM: N>1P\ETB\1035756\146251\r\1106814\984233Z\1092063\SIDS\ETB\983294\191169T$\US\1045487", emailDomain = "S\STX\ETX\51557z\DC3H[\ENQK\25943"}
testObject_Email_user_2 :: Email
testObject_Email_user_2 = Email {emailLocal = "3", emailDomain = "\61217\1091981\1111896ump5U,\145970E\22087\ap\1019776\t\53508=\1100763?P\154324\FS"}
testObject_Email_user_3 :: Email
testObject_Email_user_3 = Email {emailLocal = "\t(\1029332\NAK}\23110$\DC2\\A\"?\1034990hecAu g'Mi\996294", emailDomain = "\120249\ENQ\RS&|j\37828\t\154886\ESC;B\DLE\134912<S"}
testObject_Email_user_4 :: Email
testObject_Email_user_4 = Email {emailLocal = "IK\1107757\1002193|'Ed\155586m\142039Q\26525", emailDomain = "7\169420\987187\110835L(\NUL[_SU\1065355\1099131\SUB$\989557\1096112\SUBE\132796\&3b\RS\120667\78061}"}
testObject_Email_user_5 :: Email
testObject_Email_user_5 = Email {emailLocal = "&~", emailDomain = "\14082GMBg\NAKL\159373\1056594\137163R\62765T"}
testObject_Email_user_6 :: Email
testObject_Email_user_6 = Email {emailLocal = "]\ETBsms\23909\65518H\162084\161973\STX\92997tS\185052BDK\22348\1029379\1040486{\131461!\SO", emailDomain = "R\NAK\64475o\30203C!\nKg;l\SIF\984341\&5t\1032754m\SUB\1057588\SOH6DB\SOH"}
testObject_Email_user_7 :: Email
testObject_Email_user_7 = Email {emailLocal = "a\1050373w\1073269a\CAN45 lU\EM\1026838\&0r\EMv\SYN\RS\1079325\1009152C#gjs\SI\1047856\119939n", emailDomain = "*!\994828Z\28502V\RSg\DC1\1089841\&74"}
testObject_Email_user_8 :: Email
testObject_Email_user_8 = Email {emailLocal = "mF<\SI`2u", emailDomain = "\f7Z\DC1XrG\119839\38577/\22862\EM\ACKK\1100862~F\136098\17078\997095"}
testObject_Email_user_9 :: Email
testObject_Email_user_9 = Email {emailLocal = "#8q\13891,3^M\DLE\"4\ETXB\1022874\EM\135139\28666\162439\CANpa", emailDomain = ";Xd:Pg\184204a2w\b\187430\132238\1012147\FSC\DC2w>>\1099339\1018090"}
testObject_Email_user_10 :: Email
testObject_Email_user_10 = Email {emailLocal = "N:D3~\1062831`\DC4U\1054788\SYNm", emailDomain = "\15619\DC3\48396/827S;\DC3Fd\1031544O\1055199*i:e\135696)u\1075247\RS\995501\23545"}
testObject_Email_user_11 :: Email
testObject_Email_user_11 = Email {emailLocal = "D\\\v\1019900o\DC3\SOk*.\ENQ\STX{\15482Z\1108341\NAK\rB \DC44_m\ETX\f)6\37235", emailDomain = "\1064437VJ!.RB\1057579%+d"}
testObject_Email_user_12 :: Email
testObject_Email_user_12 = Email {emailLocal = "\DELNpz", emailDomain = ";S\DC3\ESC\187082vW"}
testObject_Email_user_13 :: Email
testObject_Email_user_13 = Email {emailLocal = "\SOR\151810\13435K\f\SI\1023124j3\1086027>\1027612\1028018mm$)1d9\US\990300W\1046864N\1034434.06", emailDomain = "\SYN\1064603\181424\1084352\31937\165330%\SO\1032305\141803\181732!Z;\177448XJ"}
testObject_Email_user_14 :: Email
testObject_Email_user_14 = Email {emailLocal = "q\DC3M\189006 T\1037628", emailDomain = "Jq8\23293iZ8_p\142943\f\152519\RS]#\29507\23899V\1020565\1068004\11998\38038{\SO\SOH\tu-"}
testObject_Email_user_15 :: Email
testObject_Email_user_15 = Email {emailLocal = "meCf\119599D>\ENQ\1034246\47966\&3\US\"\n\18804\ETBT\161977#vdgp\SUB.\161552RGf", emailDomain = "$"}
testObject_Email_user_16 :: Email
testObject_Email_user_16 = Email {emailLocal = "\996428\n^\ETXK\1077392", emailDomain = "k\1074271X\38903\&8\ESC>w"}
testObject_Email_user_17 :: Email
testObject_Email_user_17 = Email {emailLocal = "\97724\\BgJ?\f\995635\1090363}hv0|\1041624\1093136", emailDomain = ""}
testObject_Email_user_18 :: Email
testObject_Email_user_18 = Email {emailLocal = "\FSt-\STX\DC1\DLES.Z\US\EMZ\RS\1033125Ww\1087735\1021067\163282", emailDomain = "\USC\ACK5Pm)t\r^BJde\SO\53426z"}
testObject_Email_user_19 :: Email
testObject_Email_user_19 = Email {emailLocal = "S`\ESC\EOT.\DELx\1101999Qp\60010\\\ETBs\DEL{\67094\46917\DC3\DLE\SYNH\EM", emailDomain = "}*\ENQ,>\DC2"}
testObject_Email_user_20 :: Email
testObject_Email_user_20 = Email {emailLocal = "\53860s\1020303\989106leN|\61462&\ACK\48595\DC3", emailDomain = "Rz8N\180398\54005_\"w\188518\ACKB\DC4\185654\&48y\188256\997637s{\1099135[\12165"}
