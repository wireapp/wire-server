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
testObject_RichField_user_1 = RichField {richFieldType = "\1001192\98795\1087489S\GSf\20342%\FS\1082068-\vUS-\ENQ^_u;R>#\39397\DC1v", richFieldValue = "\1111118,hE\DC1\990968<Zc\RSDx\STXS"}
testObject_RichField_user_2 :: RichField
testObject_RichField_user_2 = RichField {richFieldType = "\DC4Wz<\ACKK\133546\1084896i%\DC2\1006442", richFieldValue = "S"}
testObject_RichField_user_3 :: RichField
testObject_RichField_user_3 = RichField {richFieldType = "\FS1\b4f)\ENQ\ENQ", richFieldValue = "B\1080598 jn4\NUL\SI\RSsDH\1033500"}
testObject_RichField_user_4 :: RichField
testObject_RichField_user_4 = RichField {richFieldType = "oSA\47921=\97670EpS\1024205\EOTF\FS\tX\1106293\SYN\ETXW\58546mzG\1020270&\1048573\SOH\11915", richFieldValue = "<TL4U-\991390D\39549Q\t\984080\1015629\1099917!\985873\STX\r1J"}
testObject_RichField_user_5 :: RichField
testObject_RichField_user_5 = RichField {richFieldType = "", richFieldValue = "\1002708g"}
testObject_RichField_user_6 :: RichField
testObject_RichField_user_6 = RichField {richFieldType = "\"./\"E\SI\92274,\39007\995276\59109!0y\58994\&8l", richFieldValue = "\173844\ENQ\10802q#PRAQ\ESC?\1054878\61152C\31992\DEL\146277\&5D\NULM\1059439vl\1034863Y\RS=C\SUB"}
testObject_RichField_user_7 :: RichField
testObject_RichField_user_7 = RichField {richFieldType = "\DC1\42269N\SOH\126587O^dkh- i\FSe\138824\145141j\FSN\72384X", richFieldValue = "9G+\1061153D\DLEr"}
testObject_RichField_user_8 :: RichField
testObject_RichField_user_8 = RichField {richFieldType = "\DC4", richFieldValue = "'[\1016268\23937\SIl\ar1h\52770G\RS\15120\994492"}
testObject_RichField_user_9 :: RichField
testObject_RichField_user_9 = RichField {richFieldType = "\ACK4\70165\78784\b[\180087%D", richFieldValue = "c\985701\51636z\140144\1063318"}
testObject_RichField_user_10 :: RichField
testObject_RichField_user_10 = RichField {richFieldType = "D\rk\10855\&6Jg9\985703DH\60354\a_\ETBt@Pk\1091148", richFieldValue = "j\63746\1060588\&8\1050671%\DC3 \31669\1077279\7336KY\50167\1090374\74986Oe\120087R\SOk"}
testObject_RichField_user_11 :: RichField
testObject_RichField_user_11 = RichField {richFieldType = ".\1093050A\NULH\22963)[\b", richFieldValue = "fPt\USTRbU?Zz.@l\DC3\19094\CAN\NAK\190979\f"}
testObject_RichField_user_12 :: RichField
testObject_RichField_user_12 = RichField {richFieldType = "\ENQTY\176446E\992038\&4\1035226`\1019049H", richFieldValue = "$\1070247\SI\149800R1"}
testObject_RichField_user_13 :: RichField
testObject_RichField_user_13 = RichField {richFieldType = "G~\n59\DC3m\22566\&7.I\1112467\r\RS\142312HlJYA\53468\27395/\SYNk\1021783$", richFieldValue = "\1103643\RS\ETX\22375\136175QK\1057465l\1037134f[`\ENQ\RSd"}
testObject_RichField_user_14 :: RichField
testObject_RichField_user_14 = RichField {richFieldType = "[7\NAK\142490HcQ\51270\&7SA)\"A", richFieldValue = "\1005543\ETB\DEL\1086629\EM\190837\ACKF6J\ETB"}
testObject_RichField_user_15 :: RichField
testObject_RichField_user_15 = RichField {richFieldType = "o\136606\NUL\67314c5\39918DUL\n\1108441\998049\140757\128794<\NUL?\1018002n4\EMq`", richFieldValue = "1_&\1067469v\EM/\5461h\988170=~\142197Q"}
testObject_RichField_user_16 :: RichField
testObject_RichField_user_16 = RichField {richFieldType = "\1064108Q\99619HV\120237ToYY\r\ETX\182686DK\vz_\EOTf\1008931(k\EOTI", richFieldValue = "=o\SYN&$\1064952K\f\1008220\&9\1094201T\ETX\1021986?p\170561f,\a\121311rZ\1017117\EOT\NUL&Z\1060166"}
testObject_RichField_user_17 :: RichField
testObject_RichField_user_17 = RichField {richFieldType = "\DEL\29579*L", richFieldValue = "\b\ETX\995553"}
testObject_RichField_user_18 :: RichField
testObject_RichField_user_18 = RichField {richFieldType = "\GSeG\SOHlfD\67156\154969ske\DC1_8\"", richFieldValue = "B'@x\EOT\SI\28429m\33833u=j\US&"}
testObject_RichField_user_19 :: RichField
testObject_RichField_user_19 = RichField {richFieldType = "", richFieldValue = "\100376A\1100024\USm"}
testObject_RichField_user_20 :: RichField
testObject_RichField_user_20 = RichField {richFieldType = "R\NUL\1093236\1025602\DLE\20229w\ESC\175880F\1066880N(\141712\36926\38264\161195@\DC2.J\"\171504\1090880~3q", richFieldValue = "J\rv\128045\984190\1111792\1113465"}
