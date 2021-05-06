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
testObject_AccessToken_user_1 :: AccessToken
testObject_AccessToken_user_1 = AccessToken {user = (Id (fromJust (UUID.fromString "00005309-0000-5673-0000-35cf00000700"))), access = "YC\rcd", tokenType = Bearer, expiresIn = -29}
testObject_AccessToken_user_2 :: AccessToken
testObject_AccessToken_user_2 = AccessToken {user = (Id (fromJust (UUID.fromString "00001190-0000-67fe-0000-1ec200003b44"))), access = "/\240\168\186\147rkb$7\236\178\135\244\138\168\128QoU\rS dC\EOT%\ETX]Rk\244\131\166\185s\243\188\152\166<\r\t", tokenType = Bearer, expiresIn = -17}
testObject_AccessToken_user_3 :: AccessToken
testObject_AccessToken_user_3 = AccessToken {user = (Id (fromJust (UUID.fromString "0000123e-0000-4726-0000-020c00004662"))), access = "7AOG\232\140\179d\244\133\179\135\230\169\129dI!]`\244\142\142\157\240\158\161\131]\243\179\188\191}\231\130\176\243\176\139\173(p\240\162\145\143>5\233\136\191|V", tokenType = Bearer, expiresIn = -6}
testObject_AccessToken_user_4 :: AccessToken
testObject_AccessToken_user_4 = AccessToken {user = (Id (fromJust (UUID.fromString "00001c40-0000-6097-0000-153900005662"))), access = "\DC3\ESC\240\163\147\132w.!z<\231\132\165\240\163\129\174\a$*\244\135\137\142R~\240\166\134\146f", tokenType = Bearer, expiresIn = 2}
testObject_AccessToken_user_5 :: AccessToken
testObject_AccessToken_user_5 = AccessToken {user = (Id (fromJust (UUID.fromString "00003a0d-0000-1a7e-0000-77d400004266"))), access = "3\b7G", tokenType = Bearer, expiresIn = -30}
