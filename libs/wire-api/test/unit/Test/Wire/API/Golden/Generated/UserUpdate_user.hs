{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserUpdate_user where

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
testObject_UserUpdate_1 :: UserUpdate
testObject_UserUpdate_1 = UserUpdate {uupName = Just (Name {fromName = "\SYN\20079C)\t\1069936=\1112177!\1103967b\SO1\1007205Q \1009582\FS=\34931\14009b/F\\8<\1022702\SUB\ESC=TU>\CAN\1080686\SUB\SI\149401\NUL\170920@6\995085Fe\162413*@wp,)\149001S40k\136655B\27292\&2o\1039979JD"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [], uupAccentId = Just (ColourId {fromColourId = -3})}
testObject_UserUpdate_2 :: UserUpdate
testObject_UserUpdate_2 = UserUpdate {uupName = Just (Name {fromName = "-\1032608#{\44085\DC2\b\1092904v4/\ENQ9-\DC2OFw5\\^v\"G]\167479\128065\38705\&9K#s\133134\ACKV\1033487s\175308\64659?\1066238\ESC\62090efF|~\990539G8\ahOz\STX\DELx\1059800+\f.\132843\44915;Qx7\f(^]V\12195\1102653y\1088028\131656\1094454lW`;M"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = 2})}
testObject_UserUpdate_3 :: UserUpdate
testObject_UserUpdate_3 = UserUpdate {uupName = Just (Name {fromName = "\RSc\1004361]a\SYNf\FS\74236!i\DC2\DLE:\GS7\1029797\161143\&9]\DC4\SYNVMD\EM\1076910\1112836p\155535\156543k}\DLEi\164340p\33933\vBfw,\rrp5\1102086\&7i7i\n*\1083257~Id"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "U" (Just AssetComplete)),(ImageAsset "!" (Nothing))], uupAccentId = Nothing}
testObject_UserUpdate_4 :: UserUpdate
testObject_UserUpdate_4 = UserUpdate {uupName = Just (Name {fromName = "^\18081\ENQ4\2542C;\1047073O`b_ !\151162\59257M{\995545T\ETXf\1067671N#\NAK\1055914 <\133910$\EM\1059913X;\12472Qo \1044223\&3\DEL\1032079%N@R\26353\1090511Bs6V|\1070480z?<\186541\1097112p\1071095\1109098\vwza\188355\&0\136436\4735\ETB\1105455[Pw\1034521^@P\ESC?4\63262\30721M"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "" (Just AssetPreview))], uupAccentId = Just (ColourId {fromColourId = 8})}
testObject_UserUpdate_5 :: UserUpdate
testObject_UserUpdate_5 = UserUpdate {uupName = Just (Name {fromName = "Q\1004125\151313x\1011182Xj8R6\991492>Z\DLE\1085858\SOHWrfH\1041269#f\71427\ETX@)\1017971\SYN\DC2\78772[=\7624\1019417_x>A\RS.Ai^"}), uupPict = Nothing, uupAssets = Just [(ImageAsset "" (Nothing))], uupAccentId = Just (ColourId {fromColourId = 7})}
