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
testObject_User_2eProfile_2eAsset_user_1 = (ImageAsset "]IR)\ta\EOT\"Z3" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_2 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_2 = (ImageAsset "#\ESCm0i/" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_3 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_3 = (ImageAsset "\1046311\&0\146265\13220@W-" (Nothing))
testObject_User_2eProfile_2eAsset_user_4 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_4 = (ImageAsset "\DC2`\SO\100371" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_5 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_5 = (ImageAsset "\DLE{\989110rb4\1036797\DC1K\ENQ" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_6 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_6 = (ImageAsset "\1046797No\1041022_'\62755\1099041jv,g\DC2\986969\r" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_7 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_7 = (ImageAsset "mzr\1009110" (Nothing))
testObject_User_2eProfile_2eAsset_user_8 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_8 = (ImageAsset "b\ESCN\147924\&4\US>@OY\SOHj\1014111\138192\SYN" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_9 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_9 = (ImageAsset "\138274S\DEL;\1015472\182594\1056292\1075741\SUBM\78163\96837" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_10 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_10 = (ImageAsset "\70693\989140K\DC1\1000791\1056920\53064\19122^\1026286Z\SYNC" (Nothing))
testObject_User_2eProfile_2eAsset_user_11 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_11 = (ImageAsset "(8\RSa\1079487\GS[U`BCN\bb" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_12 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_12 = (ImageAsset "\1072614\1441\DC2" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_13 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_13 = (ImageAsset "Z5\37197" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_14 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_14 = (ImageAsset "\NAKV\ENQ)\990740\1111791\1001111d+yUZ" (Just AssetPreview))
testObject_User_2eProfile_2eAsset_user_15 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_15 = (ImageAsset "\DC1\1060778\&9m\27544)\75014BX" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_16 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_16 = (ImageAsset "" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_17 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_17 = (ImageAsset "{<\147524\1089588ntK\US" (Nothing))
testObject_User_2eProfile_2eAsset_user_18 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_18 = (ImageAsset "\ENQ\USe" (Just AssetComplete))
testObject_User_2eProfile_2eAsset_user_19 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_19 = (ImageAsset "\164720\\\178946\DC1\SYNP\50415\1110769bf!\n" (Nothing))
testObject_User_2eProfile_2eAsset_user_20 :: User.Profile.Asset
testObject_User_2eProfile_2eAsset_user_20 = (ImageAsset "6r" (Nothing))
