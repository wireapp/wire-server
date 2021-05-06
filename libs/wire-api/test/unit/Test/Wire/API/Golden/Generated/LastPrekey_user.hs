{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LastPrekey_user where

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
testObject_LastPrekey_user_1 :: LastPrekey
testObject_LastPrekey_user_1 = (lastPrekey ("xv\1061016\&7\994865\ETXA\1068479\1007216\\\CAN\EM\v>\983234&"))
testObject_LastPrekey_user_2 :: LastPrekey
testObject_LastPrekey_user_2 = (lastPrekey (" \aT\EMW\RS\1061458\CAN\"\985837o\ESC\DC4\1080276\66567S!\SYNd\172109\b"))
testObject_LastPrekey_user_3 :: LastPrekey
testObject_LastPrekey_user_3 = (lastPrekey ("\"\1104421\&1.S"))
testObject_LastPrekey_user_4 :: LastPrekey
testObject_LastPrekey_user_4 = (lastPrekey ("^j\1079549\989353X\1073531F\24922z\1034953\&9_CD\1069440sdq]H\v\ax9\DC1*q"))
testObject_LastPrekey_user_5 :: LastPrekey
testObject_LastPrekey_user_5 = (lastPrekey ("\1112104Lm\v*VD^\1048224\983322Ah*c7\101067bb6(\ETXN\1109168\1028353\STX\93799\68067"))
testObject_LastPrekey_user_6 :: LastPrekey
testObject_LastPrekey_user_6 = (lastPrekey ("!\ENQ"))
testObject_LastPrekey_user_7 :: LastPrekey
testObject_LastPrekey_user_7 = (lastPrekey ("Cj\1003997|\31130\1005228\179077\ETX\59623\NUL\b\1344+\SYN\25798\FS\f!/\t\NUL\1002904)'.)zOhp"))
testObject_LastPrekey_user_8 :: LastPrekey
testObject_LastPrekey_user_8 = (lastPrekey ("\9209\9021{e'\b\RS,_\1098793\vM0C\37302;\DLEp\SO\119322m>\146100\US\ACK\CAN"))
testObject_LastPrekey_user_9 :: LastPrekey
testObject_LastPrekey_user_9 = (lastPrekey (""))
testObject_LastPrekey_user_10 :: LastPrekey
testObject_LastPrekey_user_10 = (lastPrekey ("\177208\1013539\\j,\1078013\94593F')b%\DC3\US!\FSGC$\1110625"))
testObject_LastPrekey_user_11 :: LastPrekey
testObject_LastPrekey_user_11 = (lastPrekey ("\fU\STXu\NULJD\SYN3\DC1\ACKw<BTB\180109)9\1031627\1006220\a3\1066245"))
testObject_LastPrekey_user_12 :: LastPrekey
testObject_LastPrekey_user_12 = (lastPrekey ("\1038322^<?c<#\SO\1092738.I\45642=8,b\aN%a"))
testObject_LastPrekey_user_13 :: LastPrekey
testObject_LastPrekey_user_13 = (lastPrekey ("\48311\GSWM\DC1t\ENQN8\1050153Jl\SI-\v\170466bN\97058y\DC2\171325\1001136T6"))
testObject_LastPrekey_user_14 :: LastPrekey
testObject_LastPrekey_user_14 = (lastPrekey ("\"\ENQ\1037459\&1\RSem\\\DEL\fb_D"))
testObject_LastPrekey_user_15 :: LastPrekey
testObject_LastPrekey_user_15 = (lastPrekey ("\t\SOHb\DC4.\66584p"))
testObject_LastPrekey_user_16 :: LastPrekey
testObject_LastPrekey_user_16 = (lastPrekey (":\1010468j4\SOH"))
testObject_LastPrekey_user_17 :: LastPrekey
testObject_LastPrekey_user_17 = (lastPrekey ("8D\142138lJgn\ACK$\USp\93841:,qn%R"))
testObject_LastPrekey_user_18 :: LastPrekey
testObject_LastPrekey_user_18 = (lastPrekey ("|4\1112922\ETB\1034950r\8239C\97586\177628\DC20R2R"))
testObject_LastPrekey_user_19 :: LastPrekey
testObject_LastPrekey_user_19 = (lastPrekey ("\SUB]\1085753$\1021527M\fpP\DEL%x"))
testObject_LastPrekey_user_20 :: LastPrekey
testObject_LastPrekey_user_20 = (lastPrekey ("@7\1060088%\1093204CT$]O/"))
