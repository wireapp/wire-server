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
testObject_TurnUsername_user_1 = (turnUsername (secondsToNominalDiffTime (5197419.000000000000)) ("5z7pp01oldgtjek6r84x780") & tuVersion .~ (29) & tuKeyindex .~ (14320) & tuT .~ ('\1014738'))
testObject_TurnUsername_user_2 :: TurnUsername
testObject_TurnUsername_user_2 = (turnUsername (secondsToNominalDiffTime (9156985.000000000000)) ("walwh9hbh4hnwiu") & tuVersion .~ (21) & tuKeyindex .~ (23195) & tuT .~ ('('))
testObject_TurnUsername_user_3 :: TurnUsername
testObject_TurnUsername_user_3 = (turnUsername (secondsToNominalDiffTime (3270150.000000000000)) ("2t67afi3nrsroo2v24db7dw") & tuVersion .~ (14) & tuKeyindex .~ (31035) & tuT .~ ('u'))
testObject_TurnUsername_user_4 :: TurnUsername
testObject_TurnUsername_user_4 = (turnUsername (secondsToNominalDiffTime (14806618.000000000000)) ("ry92ukk9zqdkkcqayt") & tuVersion .~ (28) & tuKeyindex .~ (4100) & tuT .~ ('\f'))
testObject_TurnUsername_user_5 :: TurnUsername
testObject_TurnUsername_user_5 = (turnUsername (secondsToNominalDiffTime (13419934.000000000000)) ("e44s") & tuVersion .~ (24) & tuKeyindex .~ (24856) & tuT .~ ('\NUL'))
