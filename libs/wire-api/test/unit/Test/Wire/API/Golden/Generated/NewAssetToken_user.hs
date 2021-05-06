{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewAssetToken_user where

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
testObject_NewAssetToken_user_1 :: NewAssetToken
testObject_NewAssetToken_user_1 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("S_g17hDrpXU=")))}}
testObject_NewAssetToken_user_2 :: NewAssetToken
testObject_NewAssetToken_user_2 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("q8g=")))}}
testObject_NewAssetToken_user_3 :: NewAssetToken
testObject_NewAssetToken_user_3 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("RCWm7KtWfKYD")))}}
testObject_NewAssetToken_user_4 :: NewAssetToken
testObject_NewAssetToken_user_4 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("Fmb_")))}}
testObject_NewAssetToken_user_5 :: NewAssetToken
testObject_NewAssetToken_user_5 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("0mh3JUCPjCsHC4fK1iRYQ6WFROQsbkWYXQE=")))}}
testObject_NewAssetToken_user_6 :: NewAssetToken
testObject_NewAssetToken_user_6 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("u6bVvqYn1wRYyBg=")))}}
testObject_NewAssetToken_user_7 :: NewAssetToken
testObject_NewAssetToken_user_7 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("BJ--oBu5ZR2hpp9DVHx7p1WNdw==")))}}
testObject_NewAssetToken_user_8 :: NewAssetToken
testObject_NewAssetToken_user_8 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("mSBNtB44hyxmiQfHds_bNbQ=")))}}
testObject_NewAssetToken_user_9 :: NewAssetToken
testObject_NewAssetToken_user_9 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("SkqMYGYO8LAKNXfHeKW8")))}}
testObject_NewAssetToken_user_10 :: NewAssetToken
testObject_NewAssetToken_user_10 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("gVlK_-QXp0KeGdQ69rminfmc")))}}
testObject_NewAssetToken_user_11 :: NewAssetToken
testObject_NewAssetToken_user_11 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("Mnbctuj172VuQnjjgdW0")))}}
testObject_NewAssetToken_user_12 :: NewAssetToken
testObject_NewAssetToken_user_12 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("jOs=")))}}
testObject_NewAssetToken_user_13 :: NewAssetToken
testObject_NewAssetToken_user_13 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("jJAs9g==")))}}
testObject_NewAssetToken_user_14 :: NewAssetToken
testObject_NewAssetToken_user_14 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("7hgwjkRlkQI=")))}}
testObject_NewAssetToken_user_15 :: NewAssetToken
testObject_NewAssetToken_user_15 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("auTFIzyi9gljoUZY8PsYww==")))}}
testObject_NewAssetToken_user_16 :: NewAssetToken
testObject_NewAssetToken_user_16 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("COzn9Gdi_iS0MqrXNktHNBDx9nprpLk=")))}}
testObject_NewAssetToken_user_17 :: NewAssetToken
testObject_NewAssetToken_user_17 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("BKabnXX7KQ==")))}}
testObject_NewAssetToken_user_18 :: NewAssetToken
testObject_NewAssetToken_user_18 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("w9o41dJJf4hu")))}}
testObject_NewAssetToken_user_19 :: NewAssetToken
testObject_NewAssetToken_user_19 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("TSpoeD5fluK2t3he7lfbXg==")))}}
testObject_NewAssetToken_user_20 :: NewAssetToken
testObject_NewAssetToken_user_20 = NewAssetToken {newAssetToken = AssetToken {assetTokenAscii = (fromRight undefined (validate ("SH_YRpo=")))}}
