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
testObject_ConversationRename_user_1 = ConversationRename {cupName = "\65305\NUL\1007240,\STXq"}
testObject_ConversationRename_user_2 :: ConversationRename
testObject_ConversationRename_user_2 = ConversationRename {cupName = "M#Z?D>1\96947>\"]\SUBy\USN}e3\1043106\1102327\1111467\1010804\187649\11699\983062\1037400\34005"}
testObject_ConversationRename_user_3 :: ConversationRename
testObject_ConversationRename_user_3 = ConversationRename {cupName = ";'F\GS\30649|\95710+"}
testObject_ConversationRename_user_4 :: ConversationRename
testObject_ConversationRename_user_4 = ConversationRename {cupName = "i\73996`1r\DC41h0N\1111938\CAN\133978`2~\ESC;!x\171194\US@\126590x\151863PO"}
testObject_ConversationRename_user_5 :: ConversationRename
testObject_ConversationRename_user_5 = ConversationRename {cupName = "}\15622\1082892\ENQg\1020692n\ENQxh\1019825:\158597Qx\f"}
testObject_ConversationRename_user_6 :: ConversationRename
testObject_ConversationRename_user_6 = ConversationRename {cupName = "\ACK\STX'Ost\1101093]\ENQNR~A\1028872J{p"}
testObject_ConversationRename_user_7 :: ConversationRename
testObject_ConversationRename_user_7 = ConversationRename {cupName = "J\43956evr\SO\&H\DELkWkk\1077875A"}
testObject_ConversationRename_user_8 :: ConversationRename
testObject_ConversationRename_user_8 = ConversationRename {cupName = "^jua\NAK UR"}
testObject_ConversationRename_user_9 :: ConversationRename
testObject_ConversationRename_user_9 = ConversationRename {cupName = "X\SOHm"}
testObject_ConversationRename_user_10 :: ConversationRename
testObject_ConversationRename_user_10 = ConversationRename {cupName = "q\DC3d)$\14768nY\1011339\\;\1089360NN\166684K|\166371B20X"}
testObject_ConversationRename_user_11 :: ConversationRename
testObject_ConversationRename_user_11 = ConversationRename {cupName = "N\DC2\v~\DC2"}
testObject_ConversationRename_user_12 :: ConversationRename
testObject_ConversationRename_user_12 = ConversationRename {cupName = "Z1\156270\DEL@j!KmF\145921"}
testObject_ConversationRename_user_13 :: ConversationRename
testObject_ConversationRename_user_13 = ConversationRename {cupName = ""}
testObject_ConversationRename_user_14 :: ConversationRename
testObject_ConversationRename_user_14 = ConversationRename {cupName = "88\ACKv\132743\154029ns\1075052\170230\CAN\35870]IP\1068005\&7\NAK\55183*\GS"}
testObject_ConversationRename_user_15 :: ConversationRename
testObject_ConversationRename_user_15 = ConversationRename {cupName = ""}
testObject_ConversationRename_user_16 :: ConversationRename
testObject_ConversationRename_user_16 = ConversationRename {cupName = "l\a\1054632\41936y%IS\992301Qt7K\133777\157778'\ETXH\1051062"}
testObject_ConversationRename_user_17 :: ConversationRename
testObject_ConversationRename_user_17 = ConversationRename {cupName = "\n"}
testObject_ConversationRename_user_18 :: ConversationRename
testObject_ConversationRename_user_18 = ConversationRename {cupName = "\bd\FS\SOHM\1076246"}
testObject_ConversationRename_user_19 :: ConversationRename
testObject_ConversationRename_user_19 = ConversationRename {cupName = "\110711OzhI8\190592`n\SO\ACK `"}
testObject_ConversationRename_user_20 :: ConversationRename
testObject_ConversationRename_user_20 = ConversationRename {cupName = "I)\24152BD\170332A<\985207\100403Z\6102^1q:|C]"}
