{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PropertyKey_user where

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
testObject_PropertyKey_user_1 :: PropertyKey
testObject_PropertyKey_user_1 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("6i0YbYmo!*?&_fF-znpPDbRW")))}
testObject_PropertyKey_user_2 :: PropertyKey
testObject_PropertyKey_user_2 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("S9?^o~b")))}
testObject_PropertyKey_user_3 :: PropertyKey
testObject_PropertyKey_user_3 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("EIn .FtINa{KK5Yg;")))}
testObject_PropertyKey_user_4 :: PropertyKey
testObject_PropertyKey_user_4 = PropertyKey {propertyKeyName = (fromRight undefined (validate (",J3[6PT_a\\SW\\(*X0GODZUcKS")))}
testObject_PropertyKey_user_5 :: PropertyKey
testObject_PropertyKey_user_5 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("?S")))}
testObject_PropertyKey_user_6 :: PropertyKey
testObject_PropertyKey_user_6 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("&[D1I_jb5b`;dpWZEQf1Pa[x")))}
testObject_PropertyKey_user_7 :: PropertyKey
testObject_PropertyKey_user_7 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("g3ra/60")))}
testObject_PropertyKey_user_8 :: PropertyKey
testObject_PropertyKey_user_8 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("Ex")))}
testObject_PropertyKey_user_9 :: PropertyKey
testObject_PropertyKey_user_9 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("*sH<!NY|8TL6vkZae,CLU\\+tApe<l")))}
testObject_PropertyKey_user_10 :: PropertyKey
testObject_PropertyKey_user_10 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("'(]x?Sh5FY]=K:#q")))}
testObject_PropertyKey_user_11 :: PropertyKey
testObject_PropertyKey_user_11 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("nzW) #/0[")))}
testObject_PropertyKey_user_12 :: PropertyKey
testObject_PropertyKey_user_12 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("HR=EjTI|7S_ei&ok")))}
testObject_PropertyKey_user_13 :: PropertyKey
testObject_PropertyKey_user_13 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("DKhp,4a9qt }")))}
testObject_PropertyKey_user_14 :: PropertyKey
testObject_PropertyKey_user_14 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("T`Fz?}\\Q2")))}
testObject_PropertyKey_user_15 :: PropertyKey
testObject_PropertyKey_user_15 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("^fk0!\"xK8M\\$062B&-(Ch8~R[*^wLh")))}
testObject_PropertyKey_user_16 :: PropertyKey
testObject_PropertyKey_user_16 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("JMH(Lg`d]-:).]Jr,]t7-<E8SRq")))}
testObject_PropertyKey_user_17 :: PropertyKey
testObject_PropertyKey_user_17 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("%\"<{qO'8^T")))}
testObject_PropertyKey_user_18 :: PropertyKey
testObject_PropertyKey_user_18 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("<H\\")))}
testObject_PropertyKey_user_19 :: PropertyKey
testObject_PropertyKey_user_19 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("]X\"bAVp!5cxeI6G8cXfx3")))}
testObject_PropertyKey_user_20 :: PropertyKey
testObject_PropertyKey_user_20 = PropertyKey {propertyKeyName = (fromRight undefined (validate ("u+Mx\"qKAvor7yP'UO/l")))}
