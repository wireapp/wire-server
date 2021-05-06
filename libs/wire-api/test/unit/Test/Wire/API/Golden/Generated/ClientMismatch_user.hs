{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ClientMismatch_user where

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
testObject_ClientMismatch_user_1 :: ClientMismatch
testObject_ClientMismatch_user_1 = (ClientMismatch (read "1864-04-10 08:40:39.285 UTC") (UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),fromList [ClientId {client = "3"}]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))),fromList [ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))),fromList [])]}) (UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000200000007"))),fromList []),((Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000700000003"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000200000008"))),fromList [ClientId {client = "1"},ClientId {client = "4"}]),((Id (fromJust (UUID.fromString "00000007-0000-0007-0000-000400000006"))),fromList [ClientId {client = "d"}]),((Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000300000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}])]}) (UserClients {userClients = fromList []}))
testObject_ClientMismatch_user_2 :: ClientMismatch
testObject_ClientMismatch_user_2 = (ClientMismatch (read "1864-05-10 15:50:26.295 UTC") (UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000004-0000-0011-0000-001d00000005"))),fromList []),((Id (fromJust (UUID.fromString "0000000e-0000-0019-0000-001500000014"))),fromList [ClientId {client = "b5"}]),((Id (fromJust (UUID.fromString "00000017-0000-0006-0000-00030000001d"))),fromList [])]}) (UserClients {userClients = fromList []}) (UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))),fromList [ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))),fromList [ClientId {client = "1"}])]}))
testObject_ClientMismatch_user_3 :: ClientMismatch
testObject_ClientMismatch_user_3 = (ClientMismatch (read "1864-04-24 04:40:09.577 UTC") (UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000018-0000-0075-0000-003500000076"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000071-0000-0043-0000-005f00000035"))),fromList [])]}) (UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000001-0000-0006-0000-000400000003"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000200000006"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000700000001"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000300000008"))),fromList []),((Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000300000004"))),fromList [ClientId {client = "f"}])]}) (UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000004"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000000000001"))),fromList [ClientId {client = "0"},ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000001"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000002"))),fromList [ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000300000001"))),fromList [ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000003"))),fromList [ClientId {client = "0"},ClientId {client = "1"}])]}))
testObject_ClientMismatch_user_4 :: ClientMismatch
testObject_ClientMismatch_user_4 = (ClientMismatch (read "1864-04-28 10:02:31.147 UTC") (UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "0000002b-0000-002d-0000-003300000042"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000044-0000-006e-0000-004a0000005b"))),fromList [ClientId {client = "1"},ClientId {client = "2"}])]}) (UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),fromList [ClientId {client = "0"},ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000300000000"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000100000003"))),fromList []),((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000200000002"))),fromList []),((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000002"))),fromList [ClientId {client = "5"}]),((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000200000004"))),fromList [ClientId {client = "0"},ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000003"))),fromList [ClientId {client = "0"},ClientId {client = "2"}])]}) (UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "000045b2-0000-24f0-0000-50c000003832"))),fromList [ClientId {client = "2"},ClientId {client = "4"},ClientId {client = "7"},ClientId {client = "8"}])]}))
testObject_ClientMismatch_user_5 :: ClientMismatch
testObject_ClientMismatch_user_5 = (ClientMismatch (read "1864-06-02 03:22:24.642 UTC") (UserClients {userClients = fromList []}) (UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000700000007"))),fromList [ClientId {client = "0"},ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000004"))),fromList [ClientId {client = "6"}]),((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000005"))),fromList []),((Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000100000008"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000000000007"))),fromList [ClientId {client = "2"}])]}) (UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))),fromList [ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))),fromList [ClientId {client = "0"}])]}))
