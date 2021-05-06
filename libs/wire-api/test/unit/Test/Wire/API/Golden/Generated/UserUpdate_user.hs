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
testObject_UserUpdate_user_1 :: UserUpdate
testObject_UserUpdate_user_1 = UserUpdate {uupName = Just (Name {fromName = "\\6 U>`\DC3\DEL(\DC1\65848Y\5816EX&[\127930yl\ENQT\EOT\NUL\ETBn\1113454K\986054\1056039 \GSQ\EOT2v>K\1053657\"\STXa\ak9\57516\1009548\&2\133759"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Just (ColourId {fromColourId = 3})}
testObject_UserUpdate_user_2 :: UserUpdate
testObject_UserUpdate_user_2 = UserUpdate {uupName = Just (Name {fromName = "\1004278 I4br\46119!\CAN~\9566_w\SId;\985123^l\8260r\1069078zm\167041\NUL:F\140496\&02\EM\63290b\47188[I\1056950\180732\1016166(h(`\57880z{ZM499\175214jJuRhw\NAK9>\SO\1022586\1071602=\NAK\ACKEP9kj\ESC;#-\129062\SUBg6E\1001557r\1089330`\ETX\SUB--@\128676\DC4yCO\157257\1029907p\11018l"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Nothing}
testObject_UserUpdate_user_3 :: UserUpdate
testObject_UserUpdate_user_3 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Just [(ImageAsset "\1069695" (Just AssetComplete)),(ImageAsset "," (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "\n" (Nothing)),(ImageAsset "PMx" (Just AssetPreview)),(ImageAsset "\1065917" (Nothing))], uupAccentId = Just (ColourId {fromColourId = -1})}
testObject_UserUpdate_user_4 :: UserUpdate
testObject_UserUpdate_user_4 = UserUpdate {uupName = Nothing, uupPict = Just (Pict {fromPict = []}), uupAssets = Nothing, uupAccentId = Nothing}
testObject_UserUpdate_user_5 :: UserUpdate
testObject_UserUpdate_user_5 = UserUpdate {uupName = Just (Name {fromName = "\f\SYNl"}), uupPict = Just (Pict {fromPict = []}), uupAssets = Just [], uupAccentId = Just (ColourId {fromColourId = 5})}
