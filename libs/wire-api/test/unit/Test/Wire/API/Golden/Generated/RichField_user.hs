{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RichField_user where

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
testObject_RichField_user_1 :: RichField
testObject_RichField_user_1 = RichField {richFieldType = "8\CAN42B]\DLE\DLEV\f:Hbla\26615\\\ACK", richFieldValue = "\152908eTCk\141807\DC3Hz\1004202C5J=\NUL\174056=(\SO\DLE\28425y[\DC3:"}
testObject_RichField_user_2 :: RichField
testObject_RichField_user_2 = RichField {richFieldType = "\60993", richFieldValue = "X\1084308\SUBV\ETBl\DC2\10046"}
testObject_RichField_user_3 :: RichField
testObject_RichField_user_3 = RichField {richFieldType = "\1021585az:\1031401DqxsRtQ\b/2b", richFieldValue = "Q\1096946\157598\FS\44650\53120D\ETX>Y\1042559U"}
testObject_RichField_user_4 :: RichField
testObject_RichField_user_4 = RichField {richFieldType = "<\r\DC29-\997405r,A&@>\1072327\1032600B\1083043\1014715\&7U\1066439\r%f\48666\20958#\36766", richFieldValue = "Ov\1035870Kb\RS&\153816\1034568'\RS!`\1096226\1057442\61718w2g|\DC4p1<1\65318"}
testObject_RichField_user_5 :: RichField
testObject_RichField_user_5 = RichField {richFieldType = "\40900", richFieldValue = "2!Q\172647;Ng\151748'\1030537\ACK\156991,v\ETB/\EMT\131798ti)K\61055"}
testObject_RichField_user_6 :: RichField
testObject_RichField_user_6 = RichField {richFieldType = "\bl", richFieldValue = "HT\41460\1084919t\DC3!\1058929\STXi\SI%\170728\\"}
testObject_RichField_user_7 :: RichField
testObject_RichField_user_7 = RichField {richFieldType = "4\156000\SOH\1017711>E\"\95927&\138862\4554w\ACK3P%\6512\124999\ACK\988880\DC32`\t", richFieldValue = "=jW"}
testObject_RichField_user_8 :: RichField
testObject_RichField_user_8 = RichField {richFieldType = "4[/\SUB_KR7\DLE!iPc\46070\1003325\DC1\1034725\133396\f\1033031A(\STXG\70507\a\SOH", richFieldValue = "\178675^&\165745\1080293"}
testObject_RichField_user_9 :: RichField
testObject_RichField_user_9 = RichField {richFieldType = ".\157477aia\DC1<\64294=gP\150358'\1044918\EM$.Xl@QkX`\1053565\136719[", richFieldValue = "*\SUBX\1098859@\152866+&\1094653/"}
testObject_RichField_user_10 :: RichField
testObject_RichField_user_10 = RichField {richFieldType = "rG\185029\b\131074\1083687\1019378;$\n@E\1015154\157680y\STXJ\1045649\ENQ", richFieldValue = "\21980p\EM\ETB"}
testObject_RichField_user_11 :: RichField
testObject_RichField_user_11 = RichField {richFieldType = "n\30001F#\174395\176168\94426W\1055140\128506", richFieldValue = "Ve\1010819"}
testObject_RichField_user_12 :: RichField
testObject_RichField_user_12 = RichField {richFieldType = "\994125d", richFieldValue = "b\CANR\147084,<\SI\EOT8\1065220\1030537G$\"V("}
testObject_RichField_user_13 :: RichField
testObject_RichField_user_13 = RichField {richFieldType = "\NUL\1079051\1003932\&6!`i\STX\1957\STXCJ)\STX\24143\1031063g\142949\133197\&6\1047862\EMZN\DC1\28422\\G@", richFieldValue = "\185790\138381\986972s%\38263\31734\1088207\1009521\37714\DC3\1072789r<\1055932\34263"}
testObject_RichField_user_14 :: RichField
testObject_RichField_user_14 = RichField {richFieldType = "R]W*\1039816(r\DC3aNy{@`+", richFieldValue = "\1035142.\f]E\n\179134]n?"}
testObject_RichField_user_15 :: RichField
testObject_RichField_user_15 = RichField {richFieldType = "\1094607\37665U]\167198}\182437= l\FSD+~\SYNO\1063390l", richFieldValue = "\20710&8\178542\16878]z\143310\&4\143879\136809\SYN\1092509\164939\vP\99034B \186983\DLE\12363ad\156487$\ACK{up"}
testObject_RichField_user_16 :: RichField
testObject_RichField_user_16 = RichField {richFieldType = "Q\EOTL^_/8xv\SUB\nqS\US", richFieldValue = "\EOT<qHuIekY"}
testObject_RichField_user_17 :: RichField
testObject_RichField_user_17 = RichField {richFieldType = "~\164874j&\STX=\DC4AO\136415\77996\1091407\NAK{\vow\SO", richFieldValue = "E8\140586\SUB\95556\1099693Y\ETB\1020323\129028"}
testObject_RichField_user_18 :: RichField
testObject_RichField_user_18 = RichField {richFieldType = ")}Bgm>\")us\1067744", richFieldValue = "\172942%it\tv\SO\37687s}K/&"}
testObject_RichField_user_19 :: RichField
testObject_RichField_user_19 = RichField {richFieldType = "\US\t\29014?~(;\RS>\DC4!\SO}\EOTZ\DC2\EOT\1020399\DELw8\t\183850\151443", richFieldValue = "\144770\1052859\173292\NAK~]cX9"}
testObject_RichField_user_20 :: RichField
testObject_RichField_user_20 = RichField {richFieldType = "O(\156660\&0\187004*hl+\44273\165875q,\b", richFieldValue = "\147072?RA\NUL"}
