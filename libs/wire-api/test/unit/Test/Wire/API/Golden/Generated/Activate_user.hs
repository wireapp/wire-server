{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Activate_user where

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
testObject_Activate_1 :: Activate
testObject_Activate_1 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "", emailDomain = "=\EMwZ\159223\1082329c"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("hPhJ-rM=")))}, activateDryrun = False}
testObject_Activate_2 :: Activate
testObject_Activate_2 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("YqY=")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("d7mOzu1E")))}, activateDryrun = True}
testObject_Activate_3 :: Activate
testObject_Activate_3 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+674122553971"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("YbBmQPo=")))}, activateDryrun = True}
testObject_Activate_4 :: Activate
testObject_Activate_4 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("BasWxG8=")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("Ww==")))}, activateDryrun = False}
testObject_Activate_5 :: Activate
testObject_Activate_5 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("p52L")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("3ZD37gPUow==")))}, activateDryrun = True}
