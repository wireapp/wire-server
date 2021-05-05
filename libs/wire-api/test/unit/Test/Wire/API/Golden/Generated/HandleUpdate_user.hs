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
testObject_HandleUpdate_1 :: HandleUpdate
testObject_HandleUpdate_1 = HandleUpdate {huHandle = "Xx\5987>\1059429\995579\1074933\1024201k\1073331\ETB#L\DEL\DC4\ncQI\1096236\1043200H\ETX\GS?"}
testObject_HandleUpdate_2 :: HandleUpdate
testObject_HandleUpdate_2 = HandleUpdate {huHandle = "\DLE=2z6\1069499\ACKG\36118\1033424\1052789q'\r\1067327"}
testObject_HandleUpdate_3 :: HandleUpdate
testObject_HandleUpdate_3 = HandleUpdate {huHandle = "\DC2W\23376\SO\SUB\EOT\49378~\"\152827R\SI\\\a\1099147Lp\19360\136433\&0"}
testObject_HandleUpdate_4 :: HandleUpdate
testObject_HandleUpdate_4 = HandleUpdate {huHandle = "\1014996\NAKjG-1\CANTV;\1100291\178486)mv"}
testObject_HandleUpdate_5 :: HandleUpdate
testObject_HandleUpdate_5 = HandleUpdate {huHandle = "X\EOT\RS0\v=w\1871;\t#\17478\180455K\1097306\NAKR\62715\169367"}
