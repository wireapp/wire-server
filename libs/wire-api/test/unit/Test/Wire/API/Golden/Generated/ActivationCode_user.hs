{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ActivationCode_user where

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
testObject_ActivationCode_user_1 :: ActivationCode
testObject_ActivationCode_user_1 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("")))}
testObject_ActivationCode_user_2 :: ActivationCode
testObject_ActivationCode_user_2 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("ZXIwNbwFp32FLbplumrffJM=")))}
testObject_ActivationCode_user_3 :: ActivationCode
testObject_ActivationCode_user_3 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("EPgJfwAZJYn7fel3yzMa8n7DOpg-")))}
testObject_ActivationCode_user_4 :: ActivationCode
testObject_ActivationCode_user_4 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("Cn0OQiU03ZL4LTphjtA=")))}
testObject_ActivationCode_user_5 :: ActivationCode
testObject_ActivationCode_user_5 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("W_mQdcESQV-Icuzn")))}
testObject_ActivationCode_user_6 :: ActivationCode
testObject_ActivationCode_user_6 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("2Qr2CTQS6PpOW0urZJM37T67wZ033oYnkaRTjg==")))}
testObject_ActivationCode_user_7 :: ActivationCode
testObject_ActivationCode_user_7 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("mQ==")))}
testObject_ActivationCode_user_8 :: ActivationCode
testObject_ActivationCode_user_8 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("umBj")))}
testObject_ActivationCode_user_9 :: ActivationCode
testObject_ActivationCode_user_9 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("NlmldSnoFqUxwFE=")))}
testObject_ActivationCode_user_10 :: ActivationCode
testObject_ActivationCode_user_10 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("8bVZleyvjpUeN2mo6Ge6ALO7OfIy")))}
testObject_ActivationCode_user_11 :: ActivationCode
testObject_ActivationCode_user_11 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("fn3z6b5B4nyIR05AvgToL_-4UXaHuA==")))}
testObject_ActivationCode_user_12 :: ActivationCode
testObject_ActivationCode_user_12 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("wWEDPTHGFwFjttLUqq3ZW4tpHg==")))}
testObject_ActivationCode_user_13 :: ActivationCode
testObject_ActivationCode_user_13 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("Oac13BKD6qI7UCZVw8iHHoSuJbS9VgOM")))}
testObject_ActivationCode_user_14 :: ActivationCode
testObject_ActivationCode_user_14 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("jc0BTYcM")))}
testObject_ActivationCode_user_15 :: ActivationCode
testObject_ActivationCode_user_15 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("9laqEnDrjxlwvVnas1O0ZhCb")))}
testObject_ActivationCode_user_16 :: ActivationCode
testObject_ActivationCode_user_16 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("N6ZWeQZ1FEL-IKD_S2lP6RR_J28Gwx-zeVA=")))}
testObject_ActivationCode_user_17 :: ActivationCode
testObject_ActivationCode_user_17 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("IQ==")))}
testObject_ActivationCode_user_18 :: ActivationCode
testObject_ActivationCode_user_18 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("CUIgRqJ-oMPfdOYmcIOqTr98fVfnjps=")))}
testObject_ActivationCode_user_19 :: ActivationCode
testObject_ActivationCode_user_19 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("uCfxEvU=")))}
testObject_ActivationCode_user_20 :: ActivationCode
testObject_ActivationCode_user_20 = ActivationCode {fromActivationCode = (fromRight undefined (validate ("YQLe4q6gfBGzWTL3KZUpgnSt")))}
