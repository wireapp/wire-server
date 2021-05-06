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
testObject_ConversationCode_user_1 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("XblWji24KaBPYUQ0Tt=R")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("_GkNy9")))))}, conversationUri = Nothing}
testObject_ConversationCode_user_2 :: ConversationCode
testObject_ConversationCode_user_2 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("4Vayb=rq7BReiesxnUk-")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("5f2b5rcipt6bakQ")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_3 :: ConversationCode
testObject_ConversationCode_user_3 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("11tR3NfBdq_vuVlS2hlR")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("I2d4noyW0cn")))))}, conversationUri = Nothing}
testObject_ConversationCode_user_4 :: ConversationCode
testObject_ConversationCode_user_4 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("GDmeSQ=MrTsSfiMnBuuY")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("RgJT8R4j68vXZ9XYU9")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_5 :: ConversationCode
testObject_ConversationCode_user_5 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("wmWVIITbmpFUexDVJIl7")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("i7qzNV94")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_6 :: ConversationCode
testObject_ConversationCode_user_6 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("wh8-2YAiytFk2DZctEHn")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("hATZiqpZNNq7")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_7 :: ConversationCode
testObject_ConversationCode_user_7 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("fBBHRmhhSGhPKkg_wndG")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("ccheFjpHUh7=W4XgBSXH")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_8 :: ConversationCode
testObject_ConversationCode_user_8 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("I3tg5nq4XqIm=dRhAY09")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("_EzKxZJF-5rGdb6EqY")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_9 :: ConversationCode
testObject_ConversationCode_user_9 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("Y9p94-fGwOC7sZPVl2IU")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("7Pwkc2caA0Tv5B")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_10 :: ConversationCode
testObject_ConversationCode_user_10 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("W2NtChnUCLFfnrF6sREx")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("UjECQ5m=")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_11 :: ConversationCode
testObject_ConversationCode_user_11 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("P0qHNpUdmhx=wpGDTaK=")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("l64hU878KT1N12")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_12 :: ConversationCode
testObject_ConversationCode_user_12 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("uhRYqThTJLM0GMGikm8=")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("84baow57NwMTgEpcs")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_13 :: ConversationCode
testObject_ConversationCode_user_13 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("bDoCcMBHr57hNlk4XCzb")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("0QRhRLu-PcRb")))))}, conversationUri = Nothing}
testObject_ConversationCode_user_14 :: ConversationCode
testObject_ConversationCode_user_14 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("69Gw8mxSDPpQxyiRiiRY")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("xiWME3=5z")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_15 :: ConversationCode
testObject_ConversationCode_user_15 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("vF0ee2RWH0EFnW6JE7wi")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("zP8iv-jMZ")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_16 :: ConversationCode
testObject_ConversationCode_user_16 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("F_ty0YA19_P9Cf0BPZBA")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("7HoRMelEzD")))))}, conversationUri = Nothing}
testObject_ConversationCode_user_17 :: ConversationCode
testObject_ConversationCode_user_17 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("liGG=YEBDfVHJJ5kAw41")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("zS9h3IebQ=4G")))))}, conversationUri = Nothing}
testObject_ConversationCode_user_18 :: ConversationCode
testObject_ConversationCode_user_18 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("M6q8YlqIZjylHndbJAM2")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("jFu6bc2m")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_19 :: ConversationCode
testObject_ConversationCode_user_19 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("t9zLNwW3uH_-kKpoPG-K")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("Q4GfUaC81lht")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_20 :: ConversationCode
testObject_ConversationCode_user_20 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("4_XZ-ww_Fo2Hh0rDor20")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("deYc0RDvEExZ5Cl")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
