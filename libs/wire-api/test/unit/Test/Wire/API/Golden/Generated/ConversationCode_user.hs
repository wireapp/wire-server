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
testObject_ConversationCode_user_1 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("2567iwS04WpTh1v5Svcy")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("3qcgDmC42V6IpK0")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_2 :: ConversationCode
testObject_ConversationCode_user_2 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("6lFanidWlNlMUr2D9OC5")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("QLKEPh")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_3 :: ConversationCode
testObject_ConversationCode_user_3 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("BAgRwlTG9RdeJJPeCZ3q")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("PpnKONbofE0YBhdb")))))}, conversationUri = Nothing}
testObject_ConversationCode_user_4 :: ConversationCode
testObject_ConversationCode_user_4 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("h3SN8T_s5pelyzqg4RNV")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("R1lekm1IZGm")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_5 :: ConversationCode
testObject_ConversationCode_user_5 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("LUDSsRISGXry=pCoQY9M")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("8IdZYWz3QN7RVMN")))))}, conversationUri = Nothing}
testObject_ConversationCode_user_6 :: ConversationCode
testObject_ConversationCode_user_6 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("9iE3Q2njWprGgH6H9maA")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("OhA0Wuo2Lv")))))}, conversationUri = Nothing}
testObject_ConversationCode_user_7 :: ConversationCode
testObject_ConversationCode_user_7 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("Piz=Up42QullyTvOXQnV")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("RfBMCatOwyjlP")))))}, conversationUri = Nothing}
testObject_ConversationCode_user_8 :: ConversationCode
testObject_ConversationCode_user_8 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("3MW=XVwe8ZZbgqo38_b7")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("wYOkkizopq")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_9 :: ConversationCode
testObject_ConversationCode_user_9 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("nG1=89rQkH0kiZHqVfJD")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("5b7Ze2La3ZPaxTDtMp")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_10 :: ConversationCode
testObject_ConversationCode_user_10 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("7HBWPLTACpsr737DHQix")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("RU2lBa0whYNT9xzUihpe")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_11 :: ConversationCode
testObject_ConversationCode_user_11 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("Wsuui2smxSWzFwX-HapQ")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("Cl_1kP1Asn")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_12 :: ConversationCode
testObject_ConversationCode_user_12 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("cxeQUQvtS6kzgX9H_Dte")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("v72tCD2F5bMLtxpEzGA")))))}, conversationUri = Nothing}
testObject_ConversationCode_user_13 :: ConversationCode
testObject_ConversationCode_user_13 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("R8=ELzjMI9iNu3vT6-I2")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("TjoyQm28KAre5JgUm0uD")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_14 :: ConversationCode
testObject_ConversationCode_user_14 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("Eurv1kopuOHn3t=UqZpU")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("a3ph=bRt9ovL")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_15 :: ConversationCode
testObject_ConversationCode_user_15 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("7cqsn2zgZFwL7MpUJrf4")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("gfxWsZAHVwj5ZH")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_16 :: ConversationCode
testObject_ConversationCode_user_16 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("nXzqlVaSInIcqjLr=gb1")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("1M81=yzXddifX")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_17 :: ConversationCode
testObject_ConversationCode_user_17 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("yraVPoOhWwX6kqQb_8s4")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("KTw2SgDIM-p")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_18 :: ConversationCode
testObject_ConversationCode_user_18 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("wvV3eDerUgsPVNY2EJWW")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("G8iqx-P3")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_19 :: ConversationCode
testObject_ConversationCode_user_19 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("Tk=NRAWNtrWU=DMMESuN")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("7P2fxCE=OzCZl")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}
testObject_ConversationCode_user_20 :: ConversationCode
testObject_ConversationCode_user_20 = ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("QV2Q=twWFkRcIolUEjnw")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("3FAcgV7y3kDaugEiVw_")))))}, conversationUri = Nothing}
