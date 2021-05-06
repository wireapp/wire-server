{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NameUpdate_user where

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
testObject_NameUpdate_user_1 :: NameUpdate
testObject_NameUpdate_user_1 = NameUpdate {nuHandle = "1\DC2\v=\NULg6\EM\1030733#\1067524\&8)H7\31858\74775._w\1090508t"}
testObject_NameUpdate_user_2 :: NameUpdate
testObject_NameUpdate_user_2 = NameUpdate {nuHandle = "\ESC"}
testObject_NameUpdate_user_3 :: NameUpdate
testObject_NameUpdate_user_3 = NameUpdate {nuHandle = "\40674r'\f\DC1ZZ\NULz0F\62989\SI\153458\SOHf8\ETB\RS%\10602\SOdn\STX\SI"}
testObject_NameUpdate_user_4 :: NameUpdate
testObject_NameUpdate_user_4 = NameUpdate {nuHandle = ""}
testObject_NameUpdate_user_5 :: NameUpdate
testObject_NameUpdate_user_5 = NameUpdate {nuHandle = "a\1074819x\21258\ETX\SUB\EM\152009\\e\141867\&8ii'5\1007180\11924"}
testObject_NameUpdate_user_6 :: NameUpdate
testObject_NameUpdate_user_6 = NameUpdate {nuHandle = "\5921\ETX?q*\1062599\v~bPk\1074418^A\EM\ACK\1061152"}
testObject_NameUpdate_user_7 :: NameUpdate
testObject_NameUpdate_user_7 = NameUpdate {nuHandle = "\NAK\987107"}
testObject_NameUpdate_user_8 :: NameUpdate
testObject_NameUpdate_user_8 = NameUpdate {nuHandle = "\158967I-\1112149B\f\194976!Mj"}
testObject_NameUpdate_user_9 :: NameUpdate
testObject_NameUpdate_user_9 = NameUpdate {nuHandle = "\STX?k \98678\CANO!\1078181Me\SYN-\190846!xrn~"}
testObject_NameUpdate_user_10 :: NameUpdate
testObject_NameUpdate_user_10 = NameUpdate {nuHandle = "5\52082Go]|1\984929\1113375/\SO@/tC\SUB\162713\&8\\Azo\ETX>"}
testObject_NameUpdate_user_11 :: NameUpdate
testObject_NameUpdate_user_11 = NameUpdate {nuHandle = "y}\1065765\1082187\NULiH"}
testObject_NameUpdate_user_12 :: NameUpdate
testObject_NameUpdate_user_12 = NameUpdate {nuHandle = "\12927mY\1018099$\178752v^z\1019976\STX\ETB{\GSS\SOHW\DC2bU]\1015758\1068348\997534\988259\US\ETB\179153"}
testObject_NameUpdate_user_13 :: NameUpdate
testObject_NameUpdate_user_13 = NameUpdate {nuHandle = ""}
testObject_NameUpdate_user_14 :: NameUpdate
testObject_NameUpdate_user_14 = NameUpdate {nuHandle = "j5\EOT\v\174832>x\n\SOH\135655\51385\146457="}
testObject_NameUpdate_user_15 :: NameUpdate
testObject_NameUpdate_user_15 = NameUpdate {nuHandle = "\1114034:=xT\140399\1017455\"\28241\5669tI^@+\181242\a\tf#\986458hG\1071470"}
testObject_NameUpdate_user_16 :: NameUpdate
testObject_NameUpdate_user_16 = NameUpdate {nuHandle = "y$w'\EM\14951\53287\&9\EM{\FS(#)N!\1024334>:U>rd"}
testObject_NameUpdate_user_17 :: NameUpdate
testObject_NameUpdate_user_17 = NameUpdate {nuHandle = "\vOrJ\160836+\ETB\SYN\177910\1073757M+\FS%e/5)\SYN>\ETX\DC3\SI,i\1031987o\99766\23169"}
testObject_NameUpdate_user_18 :: NameUpdate
testObject_NameUpdate_user_18 = NameUpdate {nuHandle = "c\US\9069\95572\1033246\SI\26501y"}
testObject_NameUpdate_user_19 :: NameUpdate
testObject_NameUpdate_user_19 = NameUpdate {nuHandle = "94r\SOHH\EOT\140292\1005140}P/i\163099\SOH\FS"}
testObject_NameUpdate_user_20 :: NameUpdate
testObject_NameUpdate_user_20 = NameUpdate {nuHandle = "\1083469\175528tn1\1074828\1027063\ESC[Vl"}
