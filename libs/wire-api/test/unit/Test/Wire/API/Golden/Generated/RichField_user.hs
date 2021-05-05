{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RichField_user where

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
testObject_RichField_1 :: RichField
testObject_RichField_1 = RichField {richFieldType = ",(\1044462z\vv\184526\SUB\1055250\1067489\&6`\120694\1094518a8\DC2w8\25046r\DC2P\1019228Z\13504\EML6", richFieldValue = "\a\DC2Y\EOTGl W\\\SO\1036717\1015313U`0_\GSBf\1014689\SUBd"}
testObject_RichField_2 :: RichField
testObject_RichField_2 = RichField {richFieldType = "\154792o.IlpR\DC4\CAN\n\136719\66840^", richFieldValue = "\183039\179373\75029\100467lbz7l\1033334\ETX6\185285\&7Ch\1036237\1013227*2"}
testObject_RichField_3 :: RichField
testObject_RichField_3 = RichField {richFieldType = "8D\DLE$\43892o\995\"9o~\1053835t", richFieldValue = "\ESC<\vU\1089795"}
testObject_RichField_4 :: RichField
testObject_RichField_4 = RichField {richFieldType = "r\tp\1032875l\176346d^\988654Q", richFieldValue = "/\t9Jw?@y\986015\&4&N\t)\987650\ETX\1052356s\EOT\2667\1107399\n9\97988~\1083460:mwh"}
testObject_RichField_5 :: RichField
testObject_RichField_5 = RichField {richFieldType = "rz\1110209\1054921\64771\&6\NULc\128597\1035187MF*fr", richFieldValue = "?3\b\n\1092417r\16874\vW\CAN\SYN`\26546\30391K|?\DC3\1059523\78117\tVYd&\"\991127gZ\CAN"}
