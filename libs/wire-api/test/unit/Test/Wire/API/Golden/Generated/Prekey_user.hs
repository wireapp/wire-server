{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Prekey_user where

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
testObject_Prekey_user_1 :: Prekey
testObject_Prekey_user_1 = Prekey {prekeyId = PrekeyId {keyId = 83}, prekeyKey = "\SO\41132T\983646s!\175008\t"}
testObject_Prekey_user_2 :: Prekey
testObject_Prekey_user_2 = Prekey {prekeyId = PrekeyId {keyId = 50}, prekeyKey = "\1048166ao+\ACK"}
testObject_Prekey_user_3 :: Prekey
testObject_Prekey_user_3 = Prekey {prekeyId = PrekeyId {keyId = 15}, prekeyKey = "\35909\1000842\"\27023"}
testObject_Prekey_user_4 :: Prekey
testObject_Prekey_user_4 = Prekey {prekeyId = PrekeyId {keyId = 77}, prekeyKey = "|\1089723\164674LH9\ACK]\1066120]s\CANy"}
testObject_Prekey_user_5 :: Prekey
testObject_Prekey_user_5 = Prekey {prekeyId = PrekeyId {keyId = 24}, prekeyKey = "Zl\83351\"'L\144224"}
testObject_Prekey_user_6 :: Prekey
testObject_Prekey_user_6 = Prekey {prekeyId = PrekeyId {keyId = 48}, prekeyKey = "\32447\132955\139054@gm\DC2,E"}
testObject_Prekey_user_7 :: Prekey
testObject_Prekey_user_7 = Prekey {prekeyId = PrekeyId {keyId = 107}, prekeyKey = "!z\14560W\100567\1097208\1105947aP"}
testObject_Prekey_user_8 :: Prekey
testObject_Prekey_user_8 = Prekey {prekeyId = PrekeyId {keyId = 101}, prekeyKey = ""}
testObject_Prekey_user_9 :: Prekey
testObject_Prekey_user_9 = Prekey {prekeyId = PrekeyId {keyId = 78}, prekeyKey = "\EOT"}
testObject_Prekey_user_10 :: Prekey
testObject_Prekey_user_10 = Prekey {prekeyId = PrekeyId {keyId = 50}, prekeyKey = "`\63368&\FSn\GS"}
testObject_Prekey_user_11 :: Prekey
testObject_Prekey_user_11 = Prekey {prekeyId = PrekeyId {keyId = 23}, prekeyKey = "\ACK\142235\&6\2466-\FSk\ESC\ETB\147511I\159603\42436\&5"}
testObject_Prekey_user_12 :: Prekey
testObject_Prekey_user_12 = Prekey {prekeyId = PrekeyId {keyId = 35}, prekeyKey = "\ENQ\DELgwT\1081332\1090488ES"}
testObject_Prekey_user_13 :: Prekey
testObject_Prekey_user_13 = Prekey {prekeyId = PrekeyId {keyId = 66}, prekeyKey = ""}
testObject_Prekey_user_14 :: Prekey
testObject_Prekey_user_14 = Prekey {prekeyId = PrekeyId {keyId = 50}, prekeyKey = ""}
testObject_Prekey_user_15 :: Prekey
testObject_Prekey_user_15 = Prekey {prekeyId = PrekeyId {keyId = 126}, prekeyKey = "\aO,>"}
testObject_Prekey_user_16 :: Prekey
testObject_Prekey_user_16 = Prekey {prekeyId = PrekeyId {keyId = 60}, prekeyKey = "3\\\RS.Z\36325"}
testObject_Prekey_user_17 :: Prekey
testObject_Prekey_user_17 = Prekey {prekeyId = PrekeyId {keyId = 30}, prekeyKey = "\EM\DC3"}
testObject_Prekey_user_18 :: Prekey
testObject_Prekey_user_18 = Prekey {prekeyId = PrekeyId {keyId = 102}, prekeyKey = "!\1096750.1Khg\DEL"}
testObject_Prekey_user_19 :: Prekey
testObject_Prekey_user_19 = Prekey {prekeyId = PrekeyId {keyId = 96}, prekeyKey = ""}
testObject_Prekey_user_20 :: Prekey
testObject_Prekey_user_20 = Prekey {prekeyId = PrekeyId {keyId = 125}, prekeyKey = "C\177701\NAK\US\RS\29126\1045610L\26244"}
