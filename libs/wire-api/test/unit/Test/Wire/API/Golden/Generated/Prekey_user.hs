{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Prekey_user where

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
testObject_Prekey_user_1 :: Prekey
testObject_Prekey_user_1 = Prekey {prekeyId = PrekeyId {keyId = 55}, prekeyKey = "e[\DC2\ETB\n3XI0R"}
testObject_Prekey_user_2 :: Prekey
testObject_Prekey_user_2 = Prekey {prekeyId = PrekeyId {keyId = 69}, prekeyKey = "[\DLEf\120503\ETB\42679\1110213\\a\1030353Pnx\155535"}
testObject_Prekey_user_3 :: Prekey
testObject_Prekey_user_3 = Prekey {prekeyId = PrekeyId {keyId = 28}, prekeyKey = "8}f<\ESC\163984\142390yD\1053931v{\DELh"}
testObject_Prekey_user_4 :: Prekey
testObject_Prekey_user_4 = Prekey {prekeyId = PrekeyId {keyId = 89}, prekeyKey = "Y\120878\&5\1012493\ETB\DC2\1082489>f\1051062T\54811.Q"}
testObject_Prekey_user_5 :: Prekey
testObject_Prekey_user_5 = Prekey {prekeyId = PrekeyId {keyId = 47}, prekeyKey = "%\1065815"}
testObject_Prekey_user_6 :: Prekey
testObject_Prekey_user_6 = Prekey {prekeyId = PrekeyId {keyId = 91}, prekeyKey = "\1073148\&87\28529\983531\9164("}
testObject_Prekey_user_7 :: Prekey
testObject_Prekey_user_7 = Prekey {prekeyId = PrekeyId {keyId = 43}, prekeyKey = "\7506\CAN\1086146\DC18\EM\SI2"}
testObject_Prekey_user_8 :: Prekey
testObject_Prekey_user_8 = Prekey {prekeyId = PrekeyId {keyId = 81}, prekeyKey = "\t\1062519uMY\ESC"}
testObject_Prekey_user_9 :: Prekey
testObject_Prekey_user_9 = Prekey {prekeyId = PrekeyId {keyId = 119}, prekeyKey = "ldG"}
testObject_Prekey_user_10 :: Prekey
testObject_Prekey_user_10 = Prekey {prekeyId = PrekeyId {keyId = 106}, prekeyKey = "X\RS!Z"}
testObject_Prekey_user_11 :: Prekey
testObject_Prekey_user_11 = Prekey {prekeyId = PrekeyId {keyId = 34}, prekeyKey = "Zj\184625\1042360\f!-\147428ma\v\ETBX\SYN5"}
testObject_Prekey_user_12 :: Prekey
testObject_Prekey_user_12 = Prekey {prekeyId = PrekeyId {keyId = 96}, prekeyKey = "^/\NAK"}
testObject_Prekey_user_13 :: Prekey
testObject_Prekey_user_13 = Prekey {prekeyId = PrekeyId {keyId = 37}, prekeyKey = "\DC1\35745wbT\98538\t\US\1102266\1043060\1016883"}
testObject_Prekey_user_14 :: Prekey
testObject_Prekey_user_14 = Prekey {prekeyId = PrekeyId {keyId = 113}, prekeyKey = "\1012593\&1t\DC4\fX0"}
testObject_Prekey_user_15 :: Prekey
testObject_Prekey_user_15 = Prekey {prekeyId = PrekeyId {keyId = 6}, prekeyKey = "g\144016<EQ\nf\1035662\SOi\176880r\161186"}
testObject_Prekey_user_16 :: Prekey
testObject_Prekey_user_16 = Prekey {prekeyId = PrekeyId {keyId = 78}, prekeyKey = "r\1108346A"}
testObject_Prekey_user_17 :: Prekey
testObject_Prekey_user_17 = Prekey {prekeyId = PrekeyId {keyId = 116}, prekeyKey = "`v1#i<"}
testObject_Prekey_user_18 :: Prekey
testObject_Prekey_user_18 = Prekey {prekeyId = PrekeyId {keyId = 76}, prekeyKey = "\1099407g"}
testObject_Prekey_user_19 :: Prekey
testObject_Prekey_user_19 = Prekey {prekeyId = PrekeyId {keyId = 97}, prekeyKey = "'1H"}
testObject_Prekey_user_20 :: Prekey
testObject_Prekey_user_20 = Prekey {prekeyId = PrekeyId {keyId = 48}, prekeyKey = "\US\SO\36298\100122\SOH\GS\1024278\EOT|\28275\&7q\NUL"}
