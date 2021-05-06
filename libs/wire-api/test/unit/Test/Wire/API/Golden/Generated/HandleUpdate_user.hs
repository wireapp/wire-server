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
testObject_HandleUpdate_user_1 = HandleUpdate {huHandle = "\917851Z\ETXH\\\48827UM?"}
testObject_HandleUpdate_user_2 :: HandleUpdate
testObject_HandleUpdate_user_2 = HandleUpdate {huHandle = "&\DC2\60598\SOH\ETXn~\142007N\19331f\100696\26499\1047969\&4u#&=XwM\1098002\&9\ETBp1k:%"}
testObject_HandleUpdate_user_3 :: HandleUpdate
testObject_HandleUpdate_user_3 = HandleUpdate {huHandle = "\n\97933"}
testObject_HandleUpdate_user_4 :: HandleUpdate
testObject_HandleUpdate_user_4 = HandleUpdate {huHandle = "\ETBOw@t7\48917-\SYN\ESCS\RS\DC3b,pw\1029938\1037801}\1496d"}
testObject_HandleUpdate_user_5 :: HandleUpdate
testObject_HandleUpdate_user_5 = HandleUpdate {huHandle = "'\186582"}
testObject_HandleUpdate_user_6 :: HandleUpdate
testObject_HandleUpdate_user_6 = HandleUpdate {huHandle = "\1110334\r\43661Lb\175299\73066\&7\n"}
testObject_HandleUpdate_user_7 :: HandleUpdate
testObject_HandleUpdate_user_7 = HandleUpdate {huHandle = "!\158965\1009302U\1062598\&1\DC1\993696^w\22185\28162\NAKWi7\NAKQQ\DEL\EM"}
testObject_HandleUpdate_user_8 :: HandleUpdate
testObject_HandleUpdate_user_8 = HandleUpdate {huHandle = "\998970K"}
testObject_HandleUpdate_user_9 :: HandleUpdate
testObject_HandleUpdate_user_9 = HandleUpdate {huHandle = "-\175176]\1050339M"}
testObject_HandleUpdate_user_10 :: HandleUpdate
testObject_HandleUpdate_user_10 = HandleUpdate {huHandle = "~y\v84&\\|\23482&I\35532Y1j\185110\995569mD"}
testObject_HandleUpdate_user_11 :: HandleUpdate
testObject_HandleUpdate_user_11 = HandleUpdate {huHandle = "I\SI\30427`F\58387\989039\1086256\160206\\\NULy\1076395w}\1019330\28603dX\v\a\DC1-}"}
testObject_HandleUpdate_user_12 :: HandleUpdate
testObject_HandleUpdate_user_12 = HandleUpdate {huHandle = "\1069480>\ETX\1054287]\CANE\992582P"}
testObject_HandleUpdate_user_13 :: HandleUpdate
testObject_HandleUpdate_user_13 = HandleUpdate {huHandle = "\1065952\a\DC3y4\58261wE\ENQMrk"}
testObject_HandleUpdate_user_14 :: HandleUpdate
testObject_HandleUpdate_user_14 = HandleUpdate {huHandle = "\94784/x}"}
testObject_HandleUpdate_user_15 :: HandleUpdate
testObject_HandleUpdate_user_15 = HandleUpdate {huHandle = "N3\ACK\EOT\SOH}\a/iJ-\98995C\1059368fi._I\DC35\35137\r\2447\1046531&"}
testObject_HandleUpdate_user_16 :: HandleUpdate
testObject_HandleUpdate_user_16 = HandleUpdate {huHandle = "w\1050274\1079229qgA.\2631"}
testObject_HandleUpdate_user_17 :: HandleUpdate
testObject_HandleUpdate_user_17 = HandleUpdate {huHandle = "y\63518\GS\EM\n+LV"}
testObject_HandleUpdate_user_18 :: HandleUpdate
testObject_HandleUpdate_user_18 = HandleUpdate {huHandle = "\985008\187965W+T\DEL#_A\SIQKb\f8\33890"}
testObject_HandleUpdate_user_19 :: HandleUpdate
testObject_HandleUpdate_user_19 = HandleUpdate {huHandle = ""}
testObject_HandleUpdate_user_20 :: HandleUpdate
testObject_HandleUpdate_user_20 = HandleUpdate {huHandle = "\":"}
