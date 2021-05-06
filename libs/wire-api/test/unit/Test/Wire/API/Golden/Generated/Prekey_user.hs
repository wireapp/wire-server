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
testObject_Prekey_user_1 = Prekey {prekeyId = PrekeyId {keyId = 116}, prekeyKey = "\DC3N\48087\50708\SUB"}
testObject_Prekey_user_2 :: Prekey
testObject_Prekey_user_2 = Prekey {prekeyId = PrekeyId {keyId = 86}, prekeyKey = "0w\SI\1000214\DC2\58269\185454h\1082157!\f\1040373"}
testObject_Prekey_user_3 :: Prekey
testObject_Prekey_user_3 = Prekey {prekeyId = PrekeyId {keyId = 68}, prekeyKey = "[ZU\DC4"}
testObject_Prekey_user_4 :: Prekey
testObject_Prekey_user_4 = Prekey {prekeyId = PrekeyId {keyId = 8}, prekeyKey = "\997508\GS1"}
testObject_Prekey_user_5 :: Prekey
testObject_Prekey_user_5 = Prekey {prekeyId = PrekeyId {keyId = 66}, prekeyKey = ""}
testObject_Prekey_user_6 :: Prekey
testObject_Prekey_user_6 = Prekey {prekeyId = PrekeyId {keyId = 103}, prekeyKey = ""}
testObject_Prekey_user_7 :: Prekey
testObject_Prekey_user_7 = Prekey {prekeyId = PrekeyId {keyId = 57}, prekeyKey = "\bVB$X"}
testObject_Prekey_user_8 :: Prekey
testObject_Prekey_user_8 = Prekey {prekeyId = PrekeyId {keyId = 128}, prekeyKey = "^cl\1003118\23838\95358@\ETX\62626 \STXG\191156\51408"}
testObject_Prekey_user_9 :: Prekey
testObject_Prekey_user_9 = Prekey {prekeyId = PrekeyId {keyId = 85}, prekeyKey = "%Y\162552\SOH\DC2g\189650{4w\v!\ENQ\50971"}
testObject_Prekey_user_10 :: Prekey
testObject_Prekey_user_10 = Prekey {prekeyId = PrekeyId {keyId = 44}, prekeyKey = "t\7991"}
testObject_Prekey_user_11 :: Prekey
testObject_Prekey_user_11 = Prekey {prekeyId = PrekeyId {keyId = 14}, prekeyKey = "\ESCbV\58414/7\1023884"}
testObject_Prekey_user_12 :: Prekey
testObject_Prekey_user_12 = Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "8}=\t"}
testObject_Prekey_user_13 :: Prekey
testObject_Prekey_user_13 = Prekey {prekeyId = PrekeyId {keyId = 127}, prekeyKey = "\1044673`\f\DC1j"}
testObject_Prekey_user_14 :: Prekey
testObject_Prekey_user_14 = Prekey {prekeyId = PrekeyId {keyId = 91}, prekeyKey = "I\DC4"}
testObject_Prekey_user_15 :: Prekey
testObject_Prekey_user_15 = Prekey {prekeyId = PrekeyId {keyId = 22}, prekeyKey = "\183933\SO"}
testObject_Prekey_user_16 :: Prekey
testObject_Prekey_user_16 = Prekey {prekeyId = PrekeyId {keyId = 85}, prekeyKey = "udG6\DLEmf\rT\4650\16753]\144852"}
testObject_Prekey_user_17 :: Prekey
testObject_Prekey_user_17 = Prekey {prekeyId = PrekeyId {keyId = 79}, prekeyKey = "~\1021017\999006"}
testObject_Prekey_user_18 :: Prekey
testObject_Prekey_user_18 = Prekey {prekeyId = PrekeyId {keyId = 16}, prekeyKey = "GD,=\bbh"}
testObject_Prekey_user_19 :: Prekey
testObject_Prekey_user_19 = Prekey {prekeyId = PrekeyId {keyId = 93}, prekeyKey = "BY\FS1\ESC\1000623V|\1028685\1035461$\26026k\155654t"}
testObject_Prekey_user_20 :: Prekey
testObject_Prekey_user_20 = Prekey {prekeyId = PrekeyId {keyId = 72}, prekeyKey = "C)\1005931\994481\&3"}
