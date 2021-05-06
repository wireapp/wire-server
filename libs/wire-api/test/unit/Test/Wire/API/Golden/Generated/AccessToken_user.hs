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
testObject_AccessToken_user_1 = AccessToken {user = (Id (fromJust (UUID.fromString "000047e4-0000-0f04-0000-05ab00007eab"))), access = "\SO~\DC4P\EOT\240\168\176\174\243\185\161\158U\USls\RS\240\166\141\137\244\135\177\138\243\187\131\187\DC1_d", tokenType = Bearer, expiresIn = -14}
testObject_AccessToken_user_2 :: AccessToken
testObject_AccessToken_user_2 = AccessToken {user = (Id (fromJust (UUID.fromString "00002097-0000-2d80-0000-00f0000077b8"))), access = "HQ'\240\160\185\128 \244\136\175\186p\244\140\163\134Y#\243\186\167\137\&2m~G\DC3K\243\186\169\178L\243\191\145\183\DEL\240\167\168\146j", tokenType = Bearer, expiresIn = 4}
testObject_AccessToken_user_3 :: AccessToken
testObject_AccessToken_user_3 = AccessToken {user = (Id (fromJust (UUID.fromString "00000566-0000-1a0f-0000-7e1800004c06"))), access = "ke\FS,d\NULrT\fd\DC2\244\141\151\130\240\161\178\173\234\181\134Cy\f\243\189\172\146:\240\166\157\191>\SYN\233\144\173\SO\\\CAN", tokenType = Bearer, expiresIn = -12}
testObject_AccessToken_user_4 :: AccessToken
testObject_AccessToken_user_4 = AccessToken {user = (Id (fromJust (UUID.fromString "00007159-0000-603f-0000-4c620000580d"))), access = "j~\\", tokenType = Bearer, expiresIn = -20}
testObject_AccessToken_user_5 :: AccessToken
testObject_AccessToken_user_5 = AccessToken {user = (Id (fromJust (UUID.fromString "000058ce-0000-4649-0000-6c6900003d6b"))), access = "\DC1B\244\139\160\144\244\137\150\176\243\188\144\186\244\130\156\160\ETX\NAKO-3\f\DC1G\244\137\158\145BU-", tokenType = Bearer, expiresIn = -1}
