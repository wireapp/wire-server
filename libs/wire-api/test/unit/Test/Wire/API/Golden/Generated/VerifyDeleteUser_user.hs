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
testObject_VerifyDeleteUser_user_1 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("Ceesjzzx15ubBQRkponG")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("5L3sKIGlWPF")))))}}
testObject_VerifyDeleteUser_user_2 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_2 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("2Nw1jy2nKcSGWQgvZyPp")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("lPaWOGV-ENbqNcMoMZ")))))}}
testObject_VerifyDeleteUser_user_3 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_3 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("Pyty2S-LGj28SI_MpTUD")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("PN3Vvh")))))}}
testObject_VerifyDeleteUser_user_4 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_4 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("of-ozUNZ_05CZTuxG=sU")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("1kqecVbC4fQdP")))))}}
testObject_VerifyDeleteUser_user_5 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_5 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("5sq6iQH5UINWgKWs8wzd")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("d9xTnKp_J=0KqR1CPnvO")))))}}
testObject_VerifyDeleteUser_user_6 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_6 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("hKwZHsCnS085ij5NzO6E")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("X8VkejOB-s0zgZ6YzI")))))}}
testObject_VerifyDeleteUser_user_7 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_7 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("7XCWnuXt8AD0TDofGiYi")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("b1D-71LwJ2Q3MQ5dP")))))}}
testObject_VerifyDeleteUser_user_8 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_8 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("bS85pjw8kEzWXUAy9xtu")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("WBuXdVNFcCTzfE")))))}}
testObject_VerifyDeleteUser_user_9 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_9 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("VkegHcx-rjDjnn6mgME9")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("ZqlwvGJV=")))))}}
testObject_VerifyDeleteUser_user_10 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_10 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("l5sTjfIEPt09Mvib78RT")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("nJ=KHHyPef2m")))))}}
testObject_VerifyDeleteUser_user_11 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_11 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("8Oxx7uIGhtz_hJiVCPXq")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("0p0SiHAXtrKn")))))}}
testObject_VerifyDeleteUser_user_12 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_12 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("EqvCbsWqvEKFPy9KHKfU")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("zVqtgiOwU")))))}}
testObject_VerifyDeleteUser_user_13 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_13 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("a0Q9Wg41Nr4GwS6ya80Z")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("c0SPojklPGi")))))}}
testObject_VerifyDeleteUser_user_14 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_14 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("FFr_F1FiR-s_j3E2Dm4H")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("WdTHCCYL4Ylq")))))}}
testObject_VerifyDeleteUser_user_15 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_15 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("dfKdyYqH-OnGQRvkbLea")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("HuBPobFRD")))))}}
testObject_VerifyDeleteUser_user_16 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_16 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("_Y38MGdKojs6JJXIh8nS")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("0c2Je6nZz88")))))}}
testObject_VerifyDeleteUser_user_17 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_17 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("2aqXAvy10qRo8HB_VLJG")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("IZWMB4lujl")))))}}
testObject_VerifyDeleteUser_user_18 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_18 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("eiDIzxLVG8dxSJjEs1qY")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("BH5Gxn7CDGi8")))))}}
testObject_VerifyDeleteUser_user_19 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_19 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("Cil6-IAFyju9WRUMkY54")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("uLOfMqEqnKWfevam1ud")))))}}
testObject_VerifyDeleteUser_user_20 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_20 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("M=4yF1PXcHqhEIvOHe6t")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("IzY9iKvfb3=")))))}}
