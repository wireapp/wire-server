{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ClientPrekey_user where

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
testObject_ClientPrekey_user_1 :: ClientPrekey
testObject_ClientPrekey_user_1 = ClientPrekey {prekeyClient = ClientId {client = "e7d"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 7}, prekeyKey = "\141660J{\1079256%"}}
testObject_ClientPrekey_user_2 :: ClientPrekey
testObject_ClientPrekey_user_2 = ClientPrekey {prekeyClient = ClientId {client = "9c4"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 8}, prekeyKey = "W \EOTD@\1072514\f"}}
testObject_ClientPrekey_user_3 :: ClientPrekey
testObject_ClientPrekey_user_3 = ClientPrekey {prekeyClient = ClientId {client = "1aa"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 6}, prekeyKey = "6t\1040090;\131687<\1061567"}}
testObject_ClientPrekey_user_4 :: ClientPrekey
testObject_ClientPrekey_user_4 = ClientPrekey {prekeyClient = ClientId {client = "a3f"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 5}, prekeyKey = "|d\1039006\t\ETX\1031771Q"}}
testObject_ClientPrekey_user_5 :: ClientPrekey
testObject_ClientPrekey_user_5 = ClientPrekey {prekeyClient = ClientId {client = "479"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 8}, prekeyKey = ""}}
testObject_ClientPrekey_user_6 :: ClientPrekey
testObject_ClientPrekey_user_6 = ClientPrekey {prekeyClient = ClientId {client = "bb"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 7}, prekeyKey = "T\1111541"}}
testObject_ClientPrekey_user_7 :: ClientPrekey
testObject_ClientPrekey_user_7 = ClientPrekey {prekeyClient = ClientId {client = "8ec"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}
testObject_ClientPrekey_user_8 :: ClientPrekey
testObject_ClientPrekey_user_8 = ClientPrekey {prekeyClient = ClientId {client = "b78"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 4}, prekeyKey = ""}}
testObject_ClientPrekey_user_9 :: ClientPrekey
testObject_ClientPrekey_user_9 = ClientPrekey {prekeyClient = ClientId {client = "e0a"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "igI"}}
testObject_ClientPrekey_user_10 :: ClientPrekey
testObject_ClientPrekey_user_10 = ClientPrekey {prekeyClient = ClientId {client = "334"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "u"}}
testObject_ClientPrekey_user_11 :: ClientPrekey
testObject_ClientPrekey_user_11 = ClientPrekey {prekeyClient = ClientId {client = "20c"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 3}, prekeyKey = "\t"}}
testObject_ClientPrekey_user_12 :: ClientPrekey
testObject_ClientPrekey_user_12 = ClientPrekey {prekeyClient = ClientId {client = "a8a"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\GS\15781Ov"}}
testObject_ClientPrekey_user_13 :: ClientPrekey
testObject_ClientPrekey_user_13 = ClientPrekey {prekeyClient = ClientId {client = "834"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 5}, prekeyKey = "0\1025787\CAN*\n"}}
testObject_ClientPrekey_user_14 :: ClientPrekey
testObject_ClientPrekey_user_14 = ClientPrekey {prekeyClient = ClientId {client = "122"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 3}, prekeyKey = "\DLE"}}
testObject_ClientPrekey_user_15 :: ClientPrekey
testObject_ClientPrekey_user_15 = ClientPrekey {prekeyClient = ClientId {client = "9f6"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\1063719"}}
testObject_ClientPrekey_user_16 :: ClientPrekey
testObject_ClientPrekey_user_16 = ClientPrekey {prekeyClient = ClientId {client = "1cd"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "u)J^!\147066x"}}
testObject_ClientPrekey_user_17 :: ClientPrekey
testObject_ClientPrekey_user_17 = ClientPrekey {prekeyClient = ClientId {client = "8b4"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\1067064\21476J\DC1F<"}}
testObject_ClientPrekey_user_18 :: ClientPrekey
testObject_ClientPrekey_user_18 = ClientPrekey {prekeyClient = ClientId {client = "a4"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\98599\1074194\1077564"}}
testObject_ClientPrekey_user_19 :: ClientPrekey
testObject_ClientPrekey_user_19 = ClientPrekey {prekeyClient = ClientId {client = "b97"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "l\35485 \DEL"}}
testObject_ClientPrekey_user_20 :: ClientPrekey
testObject_ClientPrekey_user_20 = ClientPrekey {prekeyClient = ClientId {client = "923"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "!"}}
