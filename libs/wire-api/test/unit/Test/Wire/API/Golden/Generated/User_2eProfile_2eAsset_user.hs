{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.User_2eProfile_2eAsset_user where

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
testObject_User_2eProfile_2eAsset_user_1 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_1 = (ImageAsset "\1078172w}\120182\NAKl" (Nothing))
testObject_User_2eProfile_2eAsset_user_2 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_2 = (ImageAsset "\ETX\120445t!\SUB|H\185643u\1033974\ETB" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_3 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_3 = (ImageAsset "\SIK\ESC\ENQp\1023189\CAN\v\160397\145941\13161\24260r\55030" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_4 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_4 = (ImageAsset "\17377\1019507\a:w|U8\985946" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_5 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_5 = (ImageAsset "\1106657=\NUL\179874r\ACK\1106579\&3\15958\SIC\1084689" (Nothing))
testObject_User_2eProfile_2eAsset_user_6 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_6 = (ImageAsset "w\ESC5\f~\997659" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_7 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_7 = (ImageAsset "\1047440\ESCBN?\b\GS\1095727$" (Nothing))
testObject_User_2eProfile_2eAsset_user_8 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_8 = (ImageAsset "\GS\n\984168!\1091638" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_9 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_9 = (ImageAsset "O\134816\1059261\1094159\SUBNJ\95623\1102036\174129w" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_10 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_10 = (ImageAsset "\1096127\1015467\n\126539\1042788\&3n\a_|\1029079|" (Nothing))
testObject_User_2eProfile_2eAsset_user_11 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_11 = (ImageAsset "\DC2\1040738\159292\141678R\53963A`x\1092042y" (Nothing))
testObject_User_2eProfile_2eAsset_user_12 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_12 = (ImageAsset "D\1075640>[O\9807h\tzA\1045860\n" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_13 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_13 = (ImageAsset "1\DC2\153920i\1075248;p]" (Nothing))
testObject_User_2eProfile_2eAsset_user_14 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_14 = (ImageAsset "\1082140\16215\44837\1000346s" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_15 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_15 = (ImageAsset "/\166955\991787\ETBmeogl\1006583" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_16 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_16 = (ImageAsset "\41804SDH~\173768\1023813\&3" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_17 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_17 = (ImageAsset "P\164382\ETXw\21349" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_18 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_18 = (ImageAsset "\165689\10335" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_19 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_19 = (ImageAsset "" (Nothing))
testObject_User_2eProfile_2eAsset_user_20 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_20 = (ImageAsset "rZ\147589LG0\188622]\NAKW\FS1" (Just AssetComplete))
