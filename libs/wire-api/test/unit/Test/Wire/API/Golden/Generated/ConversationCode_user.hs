{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConversationCode_user where

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
testObject_ConversationCode_user_1 :: ConversationCode
testObject_ConversationCode_user_1 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("2_aevXHnFKjcl_nPLxtI")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("hLb7tDgiP")))))}, conversationUri = Nothing}
testObject_ConversationCode_user_2 :: ConversationCode
testObject_ConversationCode_user_2 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("891COq0kW8tR5ZkcQIkA")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("QeYfcYlOV")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_3 :: ConversationCode
testObject_ConversationCode_user_3 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("gwSYmtzzFar=lKe_4PyM")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("Rh4D4Ht_")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_4 :: ConversationCode
testObject_ConversationCode_user_4 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("5VjTApaK01SkY4HBlMm5")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("DEQjc2rVIp-_m9ud6")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_5 :: ConversationCode
testObject_ConversationCode_user_5 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("SA8E3dCog0x6m9b0MqH9")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("KeH3BX")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_6 :: ConversationCode
testObject_ConversationCode_user_6 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("YMr1d7=bLuqhuW1X_YDR")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("NEFPA7nDw1AW1sex")))))}, conversationUri = Nothing}
testObject_ConversationCode_user_7 :: ConversationCode
testObject_ConversationCode_user_7 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("DPhxIMMGYRaV52Pv3h85")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("_RcNxQ043c4Untq2wh")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_8 :: ConversationCode
testObject_ConversationCode_user_8 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("tlvcX9_=S1_IVdM8skZA")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("00ic_A3MY-")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_9 :: ConversationCode
testObject_ConversationCode_user_9 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("N6yjAvj1Vww4kwMw280d")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("0TJ9BUKyNaTZ9-bZGo")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_10 :: ConversationCode
testObject_ConversationCode_user_10 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("p_yrC_G7mP-A5Dnve9FD")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("H7iMAWIrY5")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_11 :: ConversationCode
testObject_ConversationCode_user_11 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("pOrITOrSIkE2u2Qb4PvO")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("lbyyzdiSPa0LO2Pk")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_12 :: ConversationCode
testObject_ConversationCode_user_12 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("sUnTKZ4Tk6S67JPO4A_j")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("S=N25keboAS9-OlCYE_")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_13 :: ConversationCode
testObject_ConversationCode_user_13 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("B1GEsLVY6U8qfZ_7RmYu")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("pdIcjOsDDOSAxWPXy")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_14 :: ConversationCode
testObject_ConversationCode_user_14 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("hzw97vxqBHGKeSF1zl9V")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("2Y8eW0qdZuUt_Kkj3")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_15 :: ConversationCode
testObject_ConversationCode_user_15 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("X6DufymCSBQPhcvKt-jW")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("HJPKnUckPF_EyfcoP")))))}, conversationUri = Nothing}
testObject_ConversationCode_user_16 :: ConversationCode
testObject_ConversationCode_user_16 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("=lF=F_iBHFJJ3Yo=vG1c")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("vAxHV3")))))}, conversationUri = Nothing}
testObject_ConversationCode_user_17 :: ConversationCode
testObject_ConversationCode_user_17 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("P=l75KNkX8BJ6DacvtdC")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("hYrW5ME6Uk")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_18 :: ConversationCode
testObject_ConversationCode_user_18 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("9FRRLUvdpXPLxmP5P78v")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("MtWtIzwB9-NQYB_0yV")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_19 :: ConversationCode
testObject_ConversationCode_user_19 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("WaT1eX9S_ddlwl9pf3KM")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("ZKapfksyI-EA")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_20 :: ConversationCode
testObject_ConversationCode_user_20 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("HTFuH0FbTUwC0gHHs=xg")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("Of8Uv85Fl6hRH-0W4ylY")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
