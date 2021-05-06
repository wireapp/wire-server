{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.AssetToken_user where

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
testObject_AssetToken_user_1 :: AssetToken
testObject_AssetToken_user_1 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("USg=")))}
testObject_AssetToken_user_2 :: AssetToken
testObject_AssetToken_user_2 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("7cpp")))}
testObject_AssetToken_user_3 :: AssetToken
testObject_AssetToken_user_3 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("4M_cjQVkqvuT2wCK-J7DN282gOBfeV4I")))}
testObject_AssetToken_user_4 :: AssetToken
testObject_AssetToken_user_4 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("BbSK41t23IXBO7tFVgu0")))}
testObject_AssetToken_user_5 :: AssetToken
testObject_AssetToken_user_5 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("hiFfAyFecw_Su5Iz0A==")))}
testObject_AssetToken_user_6 :: AssetToken
testObject_AssetToken_user_6 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("qDGCqZivnLu-BMkO5xBBXH5NaNR5wQs=")))}
testObject_AssetToken_user_7 :: AssetToken
testObject_AssetToken_user_7 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("IGakKa0=")))}
testObject_AssetToken_user_8 :: AssetToken
testObject_AssetToken_user_8 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("Cv67dySQDQaV2x0fgKg3_qw=")))}
testObject_AssetToken_user_9 :: AssetToken
testObject_AssetToken_user_9 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("Hex6")))}
testObject_AssetToken_user_10 :: AssetToken
testObject_AssetToken_user_10 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("hIybozYKrrp5A7uyhb1F0_A02O4=")))}
testObject_AssetToken_user_11 :: AssetToken
testObject_AssetToken_user_11 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("EjQYBITeSweOsT9CI9Iw-QAGwC9YFjS6sEIyBwI=")))}
testObject_AssetToken_user_12 :: AssetToken
testObject_AssetToken_user_12 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("")))}
testObject_AssetToken_user_13 :: AssetToken
testObject_AssetToken_user_13 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("194swuI7lSPKGesLxUanGD_pr2wyH7rawCfTpu8=")))}
testObject_AssetToken_user_14 :: AssetToken
testObject_AssetToken_user_14 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("ueW2pQb_lwQ6w5cNPo8UToOR81_q")))}
testObject_AssetToken_user_15 :: AssetToken
testObject_AssetToken_user_15 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("xO_IgcEaU4kc7rbMLNQ=")))}
testObject_AssetToken_user_16 :: AssetToken
testObject_AssetToken_user_16 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("P841vg4E7aqqeg==")))}
testObject_AssetToken_user_17 :: AssetToken
testObject_AssetToken_user_17 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("9ZLM5pQWeGpVLu9N6uNtOYK6fhWj3d1u")))}
testObject_AssetToken_user_18 :: AssetToken
testObject_AssetToken_user_18 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("Pa_0eMVDqH5j_bR6b_M=")))}
testObject_AssetToken_user_19 :: AssetToken
testObject_AssetToken_user_19 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("ufqLW2pJ4XvPBcrMZw==")))}
testObject_AssetToken_user_20 :: AssetToken
testObject_AssetToken_user_20 = AssetToken {assetTokenAscii = (fromRight undefined (validate ("M7CKWQ==")))}
