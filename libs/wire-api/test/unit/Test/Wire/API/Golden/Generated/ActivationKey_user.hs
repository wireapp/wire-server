{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ActivationKey_user where

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
testObject_ActivationKey_user_1 :: ActivationKey
testObject_ActivationKey_user_1 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("oaASdtBp2W_YzxsZYK-MkErELE-FTQ==")))}
testObject_ActivationKey_user_2 :: ActivationKey
testObject_ActivationKey_user_2 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("SVJ3Ygg5J1YTV1XPuhE=")))}
testObject_ActivationKey_user_3 :: ActivationKey
testObject_ActivationKey_user_3 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("0D2IGw==")))}
testObject_ActivationKey_user_4 :: ActivationKey
testObject_ActivationKey_user_4 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("gQ47lH3cmSlcy6OYNVfW")))}
testObject_ActivationKey_user_5 :: ActivationKey
testObject_ActivationKey_user_5 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("")))}
testObject_ActivationKey_user_6 :: ActivationKey
testObject_ActivationKey_user_6 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("yREAh4b4mpGUmuuAzg==")))}
testObject_ActivationKey_user_7 :: ActivationKey
testObject_ActivationKey_user_7 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("NLVkUI6cft8Fidm__Q==")))}
testObject_ActivationKey_user_8 :: ActivationKey
testObject_ActivationKey_user_8 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("RvkH4vgATtHwQaCcIjCuW2UQT6xzWeIK")))}
testObject_ActivationKey_user_9 :: ActivationKey
testObject_ActivationKey_user_9 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("G9GKBE3kBYY91EaO0HRv0w==")))}
testObject_ActivationKey_user_10 :: ActivationKey
testObject_ActivationKey_user_10 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("84UEJxVT57xPbOVDwb-kfA==")))}
testObject_ActivationKey_user_11 :: ActivationKey
testObject_ActivationKey_user_11 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("P1gT8LUxFGwKXX5oqt3wnTo=")))}
testObject_ActivationKey_user_12 :: ActivationKey
testObject_ActivationKey_user_12 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("t2h8R49d")))}
testObject_ActivationKey_user_13 :: ActivationKey
testObject_ActivationKey_user_13 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("kJSr2js4_4T-51dvffO6HNbf")))}
testObject_ActivationKey_user_14 :: ActivationKey
testObject_ActivationKey_user_14 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("Xx5VFvIuxsigGMsGa6EhZoKUO5Q1WCM=")))}
testObject_ActivationKey_user_15 :: ActivationKey
testObject_ActivationKey_user_15 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("XgbbBPdq7C-EAQ==")))}
testObject_ActivationKey_user_16 :: ActivationKey
testObject_ActivationKey_user_16 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("AQ==")))}
testObject_ActivationKey_user_17 :: ActivationKey
testObject_ActivationKey_user_17 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("fJ-Qcy1bPt8e24gQuT4YNK8=")))}
testObject_ActivationKey_user_18 :: ActivationKey
testObject_ActivationKey_user_18 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("_7FvbSshcjVQmxo9E5UrE2VMVfufaX_TSg==")))}
testObject_ActivationKey_user_19 :: ActivationKey
testObject_ActivationKey_user_19 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("neRlNb1GJfbgKcPRCtdcNyNWYDjIp0BUBr6o0Q==")))}
testObject_ActivationKey_user_20 :: ActivationKey
testObject_ActivationKey_user_20 = ActivationKey {fromActivationKey = (fromRight undefined (validate ("x4OX")))}
