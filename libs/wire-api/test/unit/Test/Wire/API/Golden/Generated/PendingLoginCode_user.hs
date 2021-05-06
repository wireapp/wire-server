{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PendingLoginCode_user where

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
testObject_PendingLoginCode_user_1 :: PendingLoginCode
testObject_PendingLoginCode_user_1 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\1057887V<o\DC3\EOT;\STXi\1084349fz\92907fm"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (11.000000000000)))}
testObject_PendingLoginCode_user_2 :: PendingLoginCode
testObject_PendingLoginCode_user_2 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\174061]:"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-1.000000000000)))}
testObject_PendingLoginCode_user_3 :: PendingLoginCode
testObject_PendingLoginCode_user_3 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "BH(K\a\DC1\f\157848\142000Z"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-3.000000000000)))}
testObject_PendingLoginCode_user_4 :: PendingLoginCode
testObject_PendingLoginCode_user_4 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\55173\140938\146561VBUI@`=\1039414"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (7.000000000000)))}
testObject_PendingLoginCode_user_5 :: PendingLoginCode
testObject_PendingLoginCode_user_5 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\EM\STXP\137399x\SYNAsX"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (0.000000000000)))}
testObject_PendingLoginCode_user_6 :: PendingLoginCode
testObject_PendingLoginCode_user_6 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "1o\US\40653\&0X\1050707\168978"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-9.000000000000)))}
testObject_PendingLoginCode_user_7 :: PendingLoginCode
testObject_PendingLoginCode_user_7 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "C\1072324U\ESC\1063427!\160502w\1031566o$ox"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-4.000000000000)))}
testObject_PendingLoginCode_user_8 :: PendingLoginCode
testObject_PendingLoginCode_user_8 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\1106572\SI/&\SI\SIQ\1084097\a\7164\DC4\163566&^"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (13.000000000000)))}
testObject_PendingLoginCode_user_9 :: PendingLoginCode
testObject_PendingLoginCode_user_9 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\21837\&1>%\fO\1067398+G\141435w\SO\1014829"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-5.000000000000)))}
testObject_PendingLoginCode_user_10 :: PendingLoginCode
testObject_PendingLoginCode_user_10 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\DELd9\1073539\61076\78706\&3\US\19111\ETBNz"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (1.000000000000)))}
testObject_PendingLoginCode_user_11 :: PendingLoginCode
testObject_PendingLoginCode_user_11 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\1096499\SOH\128806:\SUB\nd\SUB\10406\1015248\&9\1019910\7361 ["}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (12.000000000000)))}
testObject_PendingLoginCode_user_12 :: PendingLoginCode
testObject_PendingLoginCode_user_12 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = ""}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-9.000000000000)))}
testObject_PendingLoginCode_user_13 :: PendingLoginCode
testObject_PendingLoginCode_user_13 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "R\SYNd\t\1084635\1044220\&3\148833\146152RKY"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-9.000000000000)))}
testObject_PendingLoginCode_user_14 :: PendingLoginCode
testObject_PendingLoginCode_user_14 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = ":YKLx\1113364\182712\ESCL=>\DC2"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-10.000000000000)))}
testObject_PendingLoginCode_user_15 :: PendingLoginCode
testObject_PendingLoginCode_user_15 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "Nxu\40056\&2[\a\b1G\ESCly"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-10.000000000000)))}
testObject_PendingLoginCode_user_16 :: PendingLoginCode
testObject_PendingLoginCode_user_16 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "}\SOH\ESC\DC1o\43262\170674>\1052782p\1047390"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-6.000000000000)))}
testObject_PendingLoginCode_user_17 :: PendingLoginCode
testObject_PendingLoginCode_user_17 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "8\47455M\ENQ\128309\CANf\74570\83363f\DEL"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (11.000000000000)))}
testObject_PendingLoginCode_user_18 :: PendingLoginCode
testObject_PendingLoginCode_user_18 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "D:BL"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-5.000000000000)))}
testObject_PendingLoginCode_user_19 :: PendingLoginCode
testObject_PendingLoginCode_user_19 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "\1086343\184021\1028484\97046{("}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-11.000000000000)))}
testObject_PendingLoginCode_user_20 :: PendingLoginCode
testObject_PendingLoginCode_user_20 = PendingLoginCode {pendingLoginCode = LoginCode {fromLoginCode = "T"}, pendingLoginTimeout = (Timeout (secondsToNominalDiffTime (-1.000000000000)))}
