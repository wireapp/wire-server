{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LoginId_user where

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
testObject_LoginId_user_1 :: LoginId
testObject_LoginId_user_1 = LoginByPhone (Phone {fromPhone = "+867099634970"})
testObject_LoginId_user_2 :: LoginId
testObject_LoginId_user_2 = LoginByPhone (Phone {fromPhone = "+1796359484623"})
testObject_LoginId_user_3 :: LoginId
testObject_LoginId_user_3 = LoginByPhone (Phone {fromPhone = "+621619164"})
testObject_LoginId_user_4 :: LoginId
testObject_LoginId_user_4 = LoginByHandle (Handle {fromHandle = "7jqfeut"})
testObject_LoginId_user_5 :: LoginId
testObject_LoginId_user_5 = LoginByPhone (Phone {fromPhone = "+563560619477"})
testObject_LoginId_user_6 :: LoginId
testObject_LoginId_user_6 = LoginByPhone (Phone {fromPhone = "+07188940"})
testObject_LoginId_user_7 :: LoginId
testObject_LoginId_user_7 = LoginByHandle (Handle {fromHandle = "_hmhp790"})
testObject_LoginId_user_8 :: LoginId
testObject_LoginId_user_8 = LoginByEmail (Email {emailLocal = "\RS\180234\n\bu", emailDomain = "\182180}"})
testObject_LoginId_user_9 :: LoginId
testObject_LoginId_user_9 = LoginByHandle (Handle {fromHandle = "-b8"})
testObject_LoginId_user_10 :: LoginId
testObject_LoginId_user_10 = LoginByEmail (Email {emailLocal = "", emailDomain = ".D\1006380v\1095602R)*\EMYqb\95587_LHI"})
testObject_LoginId_user_11 :: LoginId
testObject_LoginId_user_11 = LoginByHandle (Handle {fromHandle = "_e3-iv7fs-"})
testObject_LoginId_user_12 :: LoginId
testObject_LoginId_user_12 = LoginByEmail (Email {emailLocal = "\DEL,Z'\1068446S\ACK\985667\NAKU\174294", emailDomain = "\22215\62101a\1079610S\DC1\1086145\NAK\45970\DC2x|h\1048078\DLEE"})
testObject_LoginId_user_13 :: LoginId
testObject_LoginId_user_13 = LoginByPhone (Phone {fromPhone = "+80654752399"})
testObject_LoginId_user_14 :: LoginId
testObject_LoginId_user_14 = LoginByPhone (Phone {fromPhone = "+2524307165"})
testObject_LoginId_user_15 :: LoginId
testObject_LoginId_user_15 = LoginByEmail (Email {emailLocal = "7\189078\STX\\*\EOT\a%]\RS", emailDomain = "pP\986580c"})
testObject_LoginId_user_16 :: LoginId
testObject_LoginId_user_16 = LoginByHandle (Handle {fromHandle = "bpo"})
testObject_LoginId_user_17 :: LoginId
testObject_LoginId_user_17 = LoginByEmail (Email {emailLocal = "\125225\20645\993243\ENQ", emailDomain = "\10378?\FS\4005= \1090310\&4\1020549m\NAK\994633\tX\DC2I)\1042152?=5\1061363"})
testObject_LoginId_user_18 :: LoginId
testObject_LoginId_user_18 = LoginByHandle (Handle {fromHandle = "07gbqwedkm.pj6yuj0_drgpn4hnfi3rmwqb-w8wyt14rzsjvuui2.v3km7s80vzmvk1ha2092cdnkaul2elp7dqzqus9y1u5_ueblq04dazh7de67lx"})
testObject_LoginId_user_19 :: LoginId
testObject_LoginId_user_19 = LoginByPhone (Phone {fromPhone = "+507600746888484"})
testObject_LoginId_user_20 :: LoginId
testObject_LoginId_user_20 = LoginByHandle (Handle {fromHandle = "wz"})
