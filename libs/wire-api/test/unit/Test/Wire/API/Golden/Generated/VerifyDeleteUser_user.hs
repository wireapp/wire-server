{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.VerifyDeleteUser_user where

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
testObject_VerifyDeleteUser_user_1 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_1 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("ieCM=JjLNWbcbbiilGKM")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("=U5TpfW")))))}}
testObject_VerifyDeleteUser_user_2 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_2 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("KTt_HPYlW=1q-Fh0-Ron")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("u9Y-Ey8h6uMLq3bo689c")))))}}
testObject_VerifyDeleteUser_user_3 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_3 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("JF097G8WZlS8wleHZEBR")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("TlirDuyuoD8Pl")))))}}
testObject_VerifyDeleteUser_user_4 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_4 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("m9qizZhNbwyt21R79IHI")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("6b3vgNjGcgyC__52_kX")))))}}
testObject_VerifyDeleteUser_user_5 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_5 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("tgXO-CqIM=JfZgodVgQ_")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("hHwQqtM")))))}}
testObject_VerifyDeleteUser_user_6 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_6 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("oQZurDmF_AHMakIysvOm")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("bZ6TN0X4ObLTHgxbglr")))))}}
testObject_VerifyDeleteUser_user_7 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_7 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("N6TWAluvad4FnpfL21k4")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("XcO=RNycZg")))))}}
testObject_VerifyDeleteUser_user_8 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_8 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("DczBdgyHPzHt=Hxz3-NY")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("mxUiLe-nT=r=ZI")))))}}
testObject_VerifyDeleteUser_user_9 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_9 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("1KFzr_LCzq-klTPLSobx")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("6v0vp24")))))}}
testObject_VerifyDeleteUser_user_10 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_10 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("KuAbRUW1OM4KNtjeZjZh")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("FdOSOopzn2Qy")))))}}
testObject_VerifyDeleteUser_user_11 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_11 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("aR3Q4PJphF8EwV6tLEOP")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("kxU3J5e")))))}}
testObject_VerifyDeleteUser_user_12 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_12 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("f7ky8DWdw-TkRJH7ieK8")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("lGjyKiOLxqq6Kgf1")))))}}
testObject_VerifyDeleteUser_user_13 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_13 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("mPiZcXm2lSxNAJtre2Ce")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("v3AKPkqpZcE0HiPSkcBI")))))}}
testObject_VerifyDeleteUser_user_14 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_14 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("KRUBzndhpdv5RQQ3LtZD")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("ij_5ei")))))}}
testObject_VerifyDeleteUser_user_15 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_15 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("0rYh6=4dIWF_jNYzvjyR")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("ziR=2VnP4Y")))))}}
testObject_VerifyDeleteUser_user_16 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_16 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("e7ZTYnEgxq4O1alK98u-")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("iWAlo_MQ=A-3")))))}}
testObject_VerifyDeleteUser_user_17 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_17 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("eUz6uJlVeNiE8Gu-DfDc")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("E4CwQiEV3JWniNcKM_")))))}}
testObject_VerifyDeleteUser_user_18 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_18 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("M0XYfXCJa008bHn3wYgi")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("NSdCLj-beZ")))))}}
testObject_VerifyDeleteUser_user_19 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_19 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("H=rz5UstXTxohdM0BdrH")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("kUQnzVYYffVz-P6CST3")))))}}
testObject_VerifyDeleteUser_user_20 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_20 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("ybUTpWpQM4JOfpowTE4A")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("zj8_jm_Y")))))}}
