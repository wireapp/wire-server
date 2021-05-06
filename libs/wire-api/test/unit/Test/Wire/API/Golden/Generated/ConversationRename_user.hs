{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConversationRename_user where

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
testObject_ConversationRename_user_1 :: ConversationRename
testObject_ConversationRename_user_1 = ConversationRename {cupName = "f\1390\1048177\SYNI)_\EM\DC1\ESC\DC2Nz\CANJ\EMJ)s\1013556\174416\FS\1070971B(GW\157796X"}
testObject_ConversationRename_user_2 :: ConversationRename
testObject_ConversationRename_user_2 = ConversationRename {cupName = "Ak\1054065\20927\1039782qwh]\STX"}
testObject_ConversationRename_user_3 :: ConversationRename
testObject_ConversationRename_user_3 = ConversationRename {cupName = "C\150105d\128716\22090\177894Q\GSe\999018(S/>.I\26363g\172090\99330\1090469>E7r^\\\SI\US"}
testObject_ConversationRename_user_4 :: ConversationRename
testObject_ConversationRename_user_4 = ConversationRename {cupName = "chS?\DC3\1072217\1054282\7351)ey@\188704\STX^O\DC1N\63680;\US(Z"}
testObject_ConversationRename_user_5 :: ConversationRename
testObject_ConversationRename_user_5 = ConversationRename {cupName = "Q\1058031\SIa=>\1050401\SOH"}
testObject_ConversationRename_user_6 :: ConversationRename
testObject_ConversationRename_user_6 = ConversationRename {cupName = "q>\6185\\<'s\52761Z(\155968W\1108023\1031610\DC1"}
testObject_ConversationRename_user_7 :: ConversationRename
testObject_ConversationRename_user_7 = ConversationRename {cupName = "\1002628^m-\25226\149176_Lj\NUL\FS(j\DC22_N\DC2\tI^"}
testObject_ConversationRename_user_8 :: ConversationRename
testObject_ConversationRename_user_8 = ConversationRename {cupName = "\DLEZ\75065\US"}
testObject_ConversationRename_user_9 :: ConversationRename
testObject_ConversationRename_user_9 = ConversationRename {cupName = "\155147\&2\DC3\1110836VY\917857\1027852B\DLE\149803r\46552U\144965\1065711\3235R=\NULwg:"}
testObject_ConversationRename_user_10 :: ConversationRename
testObject_ConversationRename_user_10 = ConversationRename {cupName = "\SOHV\ETX\ETX\97616\&0\998235T\NAKHd\23880n]\STX\n4SI\SYN\1000442bue\984068%"}
testObject_ConversationRename_user_11 :: ConversationRename
testObject_ConversationRename_user_11 = ConversationRename {cupName = "\DC3r\"N\EMo_we\1077771\144390\DC1"}
testObject_ConversationRename_user_12 :: ConversationRename
testObject_ConversationRename_user_12 = ConversationRename {cupName = "aOv\64594+e<E:\1039589"}
testObject_ConversationRename_user_13 :: ConversationRename
testObject_ConversationRename_user_13 = ConversationRename {cupName = ".3\vO )\ESC\r\1103271p\DC1\DC4\\e&=\58254"}
testObject_ConversationRename_user_14 :: ConversationRename
testObject_ConversationRename_user_14 = ConversationRename {cupName = "7(\33421"}
testObject_ConversationRename_user_15 :: ConversationRename
testObject_ConversationRename_user_15 = ConversationRename {cupName = "5)\"I\1084093\\~\SIva!\1042857\NUL\v<d\186660=\1007251+14\CANC\ETX\\\GS7"}
testObject_ConversationRename_user_16 :: ConversationRename
testObject_ConversationRename_user_16 = ConversationRename {cupName = "'z4LA\v9\1012987\SI.\a\SO\170849\f\GS\DC2\1041439&\1020813&s\180938\DC3"}
testObject_ConversationRename_user_17 :: ConversationRename
testObject_ConversationRename_user_17 = ConversationRename {cupName = "M\\ 7\1009351\1024273y\rh\1009296\153254?&p\990055\&35S\ETXy\995768\SI\GSv\1024683h\STX\986486u\ETB"}
testObject_ConversationRename_user_18 :: ConversationRename
testObject_ConversationRename_user_18 = ConversationRename {cupName = "qy\EM.wB8\1052959)\EMW[5.!#T0m\5416\24140"}
testObject_ConversationRename_user_19 :: ConversationRename
testObject_ConversationRename_user_19 = ConversationRename {cupName = "\"[\186798n\1104074s"}
testObject_ConversationRename_user_20 :: ConversationRename
testObject_ConversationRename_user_20 = ConversationRename {cupName = "yE$T\EOT3@-l0wi1?\1078334\148174q"}
