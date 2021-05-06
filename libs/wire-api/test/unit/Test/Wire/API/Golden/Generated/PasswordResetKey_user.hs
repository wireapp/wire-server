{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PasswordResetKey_user where

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
testObject_PasswordResetKey_user_1 :: PasswordResetKey
testObject_PasswordResetKey_user_1 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("mY8DoTuljA007ncIG3P02jEolTGyxa6AMx6P")))}
testObject_PasswordResetKey_user_2 :: PasswordResetKey
testObject_PasswordResetKey_user_2 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("")))}
testObject_PasswordResetKey_user_3 :: PasswordResetKey
testObject_PasswordResetKey_user_3 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("o3A=")))}
testObject_PasswordResetKey_user_4 :: PasswordResetKey
testObject_PasswordResetKey_user_4 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("M-drgjsnK9Okxgh1NbxUOjH7ltDl8w==")))}
testObject_PasswordResetKey_user_5 :: PasswordResetKey
testObject_PasswordResetKey_user_5 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("57HeR6tFhTxe")))}
testObject_PasswordResetKey_user_6 :: PasswordResetKey
testObject_PasswordResetKey_user_6 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("fEL9AV6WmA==")))}
testObject_PasswordResetKey_user_7 :: PasswordResetKey
testObject_PasswordResetKey_user_7 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("MyphZvQUAhMK_lp4pnKR8Pm9VYd1rQ==")))}
testObject_PasswordResetKey_user_8 :: PasswordResetKey
testObject_PasswordResetKey_user_8 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("twF6")))}
testObject_PasswordResetKey_user_9 :: PasswordResetKey
testObject_PasswordResetKey_user_9 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("fEO6Kmq-0JCtStrq0EVjrA-BGgygtRXY")))}
testObject_PasswordResetKey_user_10 :: PasswordResetKey
testObject_PasswordResetKey_user_10 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("cgsqnhs=")))}
testObject_PasswordResetKey_user_11 :: PasswordResetKey
testObject_PasswordResetKey_user_11 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("tsDml2wJ")))}
testObject_PasswordResetKey_user_12 :: PasswordResetKey
testObject_PasswordResetKey_user_12 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("WnIU6j3qh1fI3c8fVHGNF6k2CegyOw==")))}
testObject_PasswordResetKey_user_13 :: PasswordResetKey
testObject_PasswordResetKey_user_13 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("u6nUSO9bLlk5pOWUiP4LLgtNNkTGbg==")))}
testObject_PasswordResetKey_user_14 :: PasswordResetKey
testObject_PasswordResetKey_user_14 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("f2GLHprwUNHk9WsNXH8tMcM9F5ogDCO2gJg=")))}
testObject_PasswordResetKey_user_15 :: PasswordResetKey
testObject_PasswordResetKey_user_15 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("n1rQFW5q")))}
testObject_PasswordResetKey_user_16 :: PasswordResetKey
testObject_PasswordResetKey_user_16 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("PrHJXLf8BYNxZ-0sZs6t")))}
testObject_PasswordResetKey_user_17 :: PasswordResetKey
testObject_PasswordResetKey_user_17 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("u1FlJJDWe1LphtUP")))}
testObject_PasswordResetKey_user_18 :: PasswordResetKey
testObject_PasswordResetKey_user_18 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("_UZSzuE__2buHJsChKiYkg==")))}
testObject_PasswordResetKey_user_19 :: PasswordResetKey
testObject_PasswordResetKey_user_19 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("")))}
testObject_PasswordResetKey_user_20 :: PasswordResetKey
testObject_PasswordResetKey_user_20 = PasswordResetKey {fromPasswordResetKey = (fromRight undefined (validate ("uvdNAhXax_81zgpl4A==")))}
