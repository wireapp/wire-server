{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TurnUsername_user where

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
testObject_TurnUsername_user_1 :: TurnUsername
testObject_TurnUsername_user_1 = (turnUsername (secondsToNominalDiffTime (4758014.000000000000)) ("n61ysadt5d0o") & tuVersion .~ (12) & tuKeyindex .~ (20171) & tuT .~ ('%'))
testObject_TurnUsername_user_2 :: TurnUsername
testObject_TurnUsername_user_2 = (turnUsername (secondsToNominalDiffTime (1672329.000000000000)) ("c3kfhtml1gdbwieespky0jr1e1") & tuVersion .~ (28) & tuKeyindex .~ (23634) & tuT .~ ('\186486'))
testObject_TurnUsername_user_3 :: TurnUsername
testObject_TurnUsername_user_3 = (turnUsername (secondsToNominalDiffTime (7234157.000000000000)) ("09og5khk68c1jlg1fp9") & tuVersion .~ (6) & tuKeyindex .~ (13466) & tuT .~ ('6'))
testObject_TurnUsername_user_4 :: TurnUsername
testObject_TurnUsername_user_4 = (turnUsername (secondsToNominalDiffTime (5471484.000000000000)) ("22bbtmc784n1v8157jrkkw7f") & tuVersion .~ (9) & tuKeyindex .~ (7430) & tuT .~ ('\184488'))
testObject_TurnUsername_user_5 :: TurnUsername
testObject_TurnUsername_user_5 = (turnUsername (secondsToNominalDiffTime (5251177.000000000000)) ("8puq3xwkuos2qyp53owoc9") & tuVersion .~ (0) & tuKeyindex .~ (356) & tuT .~ ('\SI'))
