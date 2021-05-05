{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.AccessToken_user where

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
testObject_AccessToken_1 :: AccessToken
testObject_AccessToken_1 = AccessToken {user = (Id (fromJust (UUID.fromString "00004ec0-0000-7a65-0000-44970000699b"))), access = "%=", tokenType = Bearer, expiresIn = -15}
testObject_AccessToken_2 :: AccessToken
testObject_AccessToken_2 = AccessToken {user = (Id (fromJust (UUID.fromString "00004fa5-0000-55e1-0000-084000001659"))), access = "\ETX5\240\170\165\191\DC2\244\130\133\190\231\159\155M\240\166\151\161T\b\DC4]", tokenType = Bearer, expiresIn = -23}
testObject_AccessToken_3 :: AccessToken
testObject_AccessToken_3 = AccessToken {user = (Id (fromJust (UUID.fromString "00002604-0000-0e7a-0000-18b300007b7e"))), access = "\ETXu/hzK2*\EM\r)\GS\243\190\184\161fv\CANm\235\130\140(", tokenType = Bearer, expiresIn = 26}
testObject_AccessToken_4 :: AccessToken
testObject_AccessToken_4 = AccessToken {user = (Id (fromJust (UUID.fromString "00007bd2-0000-3d1d-0000-01aa00003198"))), access = "\240\167\189\163zC\243\177\171\153\DC2Yh\t", tokenType = Bearer, expiresIn = 17}
testObject_AccessToken_5 :: AccessToken
testObject_AccessToken_5 = AccessToken {user = (Id (fromJust (UUID.fromString "00001fab-0000-7bea-0000-1e7f00006fc7"))), access = "\240\146\128\174>9+Cf", tokenType = Bearer, expiresIn = -16}
