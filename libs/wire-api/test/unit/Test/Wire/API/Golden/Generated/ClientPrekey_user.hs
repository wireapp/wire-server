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
testObject_ClientPrekey_user_1 = ClientPrekey {prekeyClient = ClientId {client = "63c"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 8}, prekeyKey = "\SUB&ef1|P"}}
testObject_ClientPrekey_user_2 :: ClientPrekey
testObject_ClientPrekey_user_2 = ClientPrekey {prekeyClient = ClientId {client = "d42"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 8}, prekeyKey = "=\17227?T"}}
testObject_ClientPrekey_user_3 :: ClientPrekey
testObject_ClientPrekey_user_3 = ClientPrekey {prekeyClient = ClientId {client = "879"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 4}, prekeyKey = "["}}
testObject_ClientPrekey_user_4 :: ClientPrekey
testObject_ClientPrekey_user_4 = ClientPrekey {prekeyClient = ClientId {client = "d31"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 7}, prekeyKey = "l\\\1060898&"}}
testObject_ClientPrekey_user_5 :: ClientPrekey
testObject_ClientPrekey_user_5 = ClientPrekey {prekeyClient = ClientId {client = "bf3"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 3}, prekeyKey = "6(eY_\STX"}}
testObject_ClientPrekey_user_6 :: ClientPrekey
testObject_ClientPrekey_user_6 = ClientPrekey {prekeyClient = ClientId {client = "e4e"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 5}, prekeyKey = "\1087193\GS\ESC-"}}
testObject_ClientPrekey_user_7 :: ClientPrekey
testObject_ClientPrekey_user_7 = ClientPrekey {prekeyClient = ClientId {client = "58e"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 6}, prekeyKey = ""}}
testObject_ClientPrekey_user_8 :: ClientPrekey
testObject_ClientPrekey_user_8 = ClientPrekey {prekeyClient = ClientId {client = "81d"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 6}, prekeyKey = ""}}
testObject_ClientPrekey_user_9 :: ClientPrekey
testObject_ClientPrekey_user_9 = ClientPrekey {prekeyClient = ClientId {client = "17"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 5}, prekeyKey = "\15117\DC4N"}}
testObject_ClientPrekey_user_10 :: ClientPrekey
testObject_ClientPrekey_user_10 = ClientPrekey {prekeyClient = ClientId {client = "741"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 6}, prekeyKey = "\FS\EOT_"}}
testObject_ClientPrekey_user_11 :: ClientPrekey
testObject_ClientPrekey_user_11 = ClientPrekey {prekeyClient = ClientId {client = "c67"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 7}, prekeyKey = ""}}
testObject_ClientPrekey_user_12 :: ClientPrekey
testObject_ClientPrekey_user_12 = ClientPrekey {prekeyClient = ClientId {client = "964"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 5}, prekeyKey = ""}}
testObject_ClientPrekey_user_13 :: ClientPrekey
testObject_ClientPrekey_user_13 = ClientPrekey {prekeyClient = ClientId {client = "4ef"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 4}, prekeyKey = "yV"}}
testObject_ClientPrekey_user_14 :: ClientPrekey
testObject_ClientPrekey_user_14 = ClientPrekey {prekeyClient = ClientId {client = "284"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 6}, prekeyKey = "\135163\&8Z"}}
testObject_ClientPrekey_user_15 :: ClientPrekey
testObject_ClientPrekey_user_15 = ClientPrekey {prekeyClient = ClientId {client = "ab6"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 7}, prekeyKey = "\n\1069864"}}
testObject_ClientPrekey_user_16 :: ClientPrekey
testObject_ClientPrekey_user_16 = ClientPrekey {prekeyClient = ClientId {client = "579"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1056777\1096069.\FS\DLE\n\SOH"}}
testObject_ClientPrekey_user_17 :: ClientPrekey
testObject_ClientPrekey_user_17 = ClientPrekey {prekeyClient = ClientId {client = "8d3"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 7}, prekeyKey = "\1547-'"}}
testObject_ClientPrekey_user_18 :: ClientPrekey
testObject_ClientPrekey_user_18 = ClientPrekey {prekeyClient = ClientId {client = "603"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "%\188686\1021085u\1019746\t\""}}
testObject_ClientPrekey_user_19 :: ClientPrekey
testObject_ClientPrekey_user_19 = ClientPrekey {prekeyClient = ClientId {client = "beb"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 3}, prekeyKey = "\NAK\te\f\51164"}}
testObject_ClientPrekey_user_20 :: ClientPrekey
testObject_ClientPrekey_user_20 = ClientPrekey {prekeyClient = ClientId {client = "a48"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 3}, prekeyKey = "\989667\STXc"}}
