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
testObject_VerifyDeleteUser_user_1 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("1KBdOGVt_o0RpJbrFQ-S")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("kfJ3uS")))))}}
testObject_VerifyDeleteUser_user_2 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_2 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("m3iUNtLeL6kwOyICf4h4")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("EHZ8wpCn-o-xzi4Bolk")))))}}
testObject_VerifyDeleteUser_user_3 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_3 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("Snc8XwOGCz_3IcysGdrw")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("_L=-RMlY=kn9JD-4O")))))}}
testObject_VerifyDeleteUser_user_4 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_4 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("s2pfXkp5fYltnO3o7Qef")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("8ajIo_2mgKE6QI")))))}}
testObject_VerifyDeleteUser_user_5 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_5 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("WT-sas3z1_PD2ZtBlHyW")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("nNSvUZQ8E")))))}}
testObject_VerifyDeleteUser_user_6 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_6 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("QN1bnH15uA09sJXVQZ6G")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("wsaISxQSWxGCS3")))))}}
testObject_VerifyDeleteUser_user_7 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_7 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("IhXYS-txBVpAZu7CGZzF")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("GHeyIQWX4f")))))}}
testObject_VerifyDeleteUser_user_8 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_8 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("CW0BgWFpeklZoXfu3_hc")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("rVeZO1LiqFGJ")))))}}
testObject_VerifyDeleteUser_user_9 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_9 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("9qxeyZ5xi2oibiZLxUNz")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("qVArAkW8")))))}}
testObject_VerifyDeleteUser_user_10 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_10 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("pxs=R_Jj2_ZBQyumeDSQ")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("viH4PKPhx419OW-VLI")))))}}
testObject_VerifyDeleteUser_user_11 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_11 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("yKzIhWdpL9vgbnDOI7yx")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("t_J-wlclYUbwss-3vv")))))}}
testObject_VerifyDeleteUser_user_12 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_12 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("y=aSgVt91nITwk=Dx=eN")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("XSCyhRl")))))}}
testObject_VerifyDeleteUser_user_13 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_13 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("9OW6On1sUQxu1S57OJXF")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("FbsXJkQeKPJxmepbVeYx")))))}}
testObject_VerifyDeleteUser_user_14 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_14 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("atAOQN8su0PB3qOaUhwd")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("pAOzD_m=wGVkJz")))))}}
testObject_VerifyDeleteUser_user_15 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_15 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("v7A3vGe=6h4tuLHx39Ym")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("1R9OXeFwTiBsj6ik")))))}}
testObject_VerifyDeleteUser_user_16 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_16 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("tHPmS3f6IaGsA4Yu1uZ8")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("AzcD49L6")))))}}
testObject_VerifyDeleteUser_user_17 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_17 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("1P8PkaMMoLGq-k5IQTc6")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("LY3RMOSzAxXO-")))))}}
testObject_VerifyDeleteUser_user_18 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_18 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("wInZ8FT_ilDPGb0ZSUlD")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("zegEBuZy")))))}}
testObject_VerifyDeleteUser_user_19 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_19 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("GVFboTwXLiUwcQq9scPP")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("WxR6rBWtZ")))))}}
testObject_VerifyDeleteUser_user_20 :: VerifyDeleteUser
testObject_VerifyDeleteUser_user_20 = VerifyDeleteUser {verifyDeleteUserKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("vRNnQXhylbPb0TofX35S")))))}, verifyDeleteUserCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("BP7BJ5OUj")))))}}
