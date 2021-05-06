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
testObject_User_2eProfile_2eAsset_user_1 = (ImageAsset "" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_2 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_2 = (ImageAsset "F" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_3 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_3 = (ImageAsset "c" (Nothing))
testObject_User_2eProfile_2eAsset_user_4 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_4 = (ImageAsset "\31371\a\US" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_5 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_5 = (ImageAsset "Y!!\1000815S\42360\42407" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_6 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_6 = (ImageAsset "G\SOHe\459=t]" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_7 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_7 = (ImageAsset "X\DLE8" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_8 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_8 = (ImageAsset "\ETXBDP\vU)S\988655Y-nf-J" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_9 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_9 = (ImageAsset "b'`\EOTgd\38535^>\74921_\GS" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_10 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_10 = (ImageAsset "}+:-\FSl\170226\1001138S\n\94911" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_11 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_11 = (ImageAsset "=\20309\1075803" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_12 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_12 = (ImageAsset "\1086491J.O\DC1\163632\&3,q_\97461LI\ACK4" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_13 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_13 = (ImageAsset "&f\EM\51296\145461}i" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_14 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_14 = (ImageAsset "\ETB\1048467/\DC2" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_15 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_15 = (ImageAsset "iNxe\1109500tm" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_16 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_16 = (ImageAsset "\STX\fVq\1022344t<s\994764\36407\DC2\46346L\FS/" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_17 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_17 = (ImageAsset "\1065135\1003999\DEL4K\\#5\1065552J\ETX)" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_18 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_18 = (ImageAsset "\191135" (Nothing))
testObject_User_2eProfile_2eAsset_user_19 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_19 = (ImageAsset "\ETBl`}p\1062528\ETX" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_20 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_20 = (ImageAsset "\f@a&T\SI\1112989DX\1091361\ESC" (Just AssetComplete))
