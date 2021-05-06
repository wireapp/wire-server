{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.HandleUpdate_user where

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
testObject_HandleUpdate_user_1 :: HandleUpdate
testObject_HandleUpdate_user_1 = HandleUpdate {huHandle = "J\152366/\n:j_y\EOT^mC\FSw\b\t"}
testObject_HandleUpdate_user_2 :: HandleUpdate
testObject_HandleUpdate_user_2 = HandleUpdate {huHandle = "H~-\SUB>.\EOT\t\23153\994753=\DEL7~\23724>$LjS#k \RSgo\DC4;z"}
testObject_HandleUpdate_user_3 :: HandleUpdate
testObject_HandleUpdate_user_3 = HandleUpdate {huHandle = "uU[`"}
testObject_HandleUpdate_user_4 :: HandleUpdate
testObject_HandleUpdate_user_4 = HandleUpdate {huHandle = "\19272\NAKH\44803\&0_{\133417\1036293\74299c\CANl\GS*\DC3\158337!ox\SUB\DEL\EM2,\1026283\GSg "}
testObject_HandleUpdate_user_5 :: HandleUpdate
testObject_HandleUpdate_user_5 = HandleUpdate {huHandle = "\SUBCsO|\27229\NAK\2921\37802&\"TW\ESC"}
testObject_HandleUpdate_user_6 :: HandleUpdate
testObject_HandleUpdate_user_6 = HandleUpdate {huHandle = "\16442tU\1030688\1038779\FS\133677\SI\1040868n\SYN"}
testObject_HandleUpdate_user_7 :: HandleUpdate
testObject_HandleUpdate_user_7 = HandleUpdate {huHandle = "6bx!\NAK\181046,\DEL:\4342,\162505W\27525\39307\32064\1106573 \133489[?2!\CAN+&X=\ACKs"}
testObject_HandleUpdate_user_8 :: HandleUpdate
testObject_HandleUpdate_user_8 = HandleUpdate {huHandle = "E$\"h\1024538HU)v%\DC4]x\132657"}
testObject_HandleUpdate_user_9 :: HandleUpdate
testObject_HandleUpdate_user_9 = HandleUpdate {huHandle = "\EOT\985876\175083M}Z?m\1020097\15632\\\1090897)\144512'(z2&[\100585\996459N\by\STX\FS\78374"}
testObject_HandleUpdate_user_10 :: HandleUpdate
testObject_HandleUpdate_user_10 = HandleUpdate {huHandle = " \a"}
testObject_HandleUpdate_user_11 :: HandleUpdate
testObject_HandleUpdate_user_11 = HandleUpdate {huHandle = "\DC2\EOTN\SYN\DC1E"}
testObject_HandleUpdate_user_12 :: HandleUpdate
testObject_HandleUpdate_user_12 = HandleUpdate {huHandle = "\ETB\186909\95211+\bd\EOT\EOT\1089702\73111/=[<6\v\1052675k?o"}
testObject_HandleUpdate_user_13 :: HandleUpdate
testObject_HandleUpdate_user_13 = HandleUpdate {huHandle = "+)B\1028947\&3;\n}\1005225\997630\&4\ESC8X\1031050\1087799U`}\DLEYP\174936"}
testObject_HandleUpdate_user_14 :: HandleUpdate
testObject_HandleUpdate_user_14 = HandleUpdate {huHandle = "\r\994919\v\ENQ;+z\1034846F<^\1046673\181784\1055502(N[\170407?h\ENQ`"}
testObject_HandleUpdate_user_15 :: HandleUpdate
testObject_HandleUpdate_user_15 = HandleUpdate {huHandle = "S\NAKyp\ETBg"}
testObject_HandleUpdate_user_16 :: HandleUpdate
testObject_HandleUpdate_user_16 = HandleUpdate {huHandle = "wo\1052270\NAK$w\RS\163496\171068\SYNBp\EOT\GS\58210\US\1024925\1070026N\984533\1075964"}
testObject_HandleUpdate_user_17 :: HandleUpdate
testObject_HandleUpdate_user_17 = HandleUpdate {huHandle = "8"}
testObject_HandleUpdate_user_18 :: HandleUpdate
testObject_HandleUpdate_user_18 = HandleUpdate {huHandle = "A\DC3\SUB;\r\1105188\1013232v\US\136891`[\DC1VbWyt\n\96528\132200L$\SYN\13690\1011208hH%i"}
testObject_HandleUpdate_user_19 :: HandleUpdate
testObject_HandleUpdate_user_19 = HandleUpdate {huHandle = "\SUB_\aT"}
testObject_HandleUpdate_user_20 :: HandleUpdate
testObject_HandleUpdate_user_20 = HandleUpdate {huHandle = "\27209sA\1091970_\96680_\ACK\1017726p\DEL\1916\1112239\92566\36901\NAKo\1066917\194829\EOTu\170084\155493\&8b\1010293[\1022920"}
