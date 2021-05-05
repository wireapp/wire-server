{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.QueuedNotificationList_user where

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
testObject_QueuedNotificationList_1 :: QueuedNotificationList
testObject_QueuedNotificationList_1 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [("\r",Object (fromList [("1",String "#")])),("[=",Object (fromList []))],fromList [],fromList [],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002")))) ((List1 (NonEmpty.fromList [fromList [("\ESC`",Array [Bool True,Null,Null,Null])]]))))]) (False) (fmap read (Nothing)))
testObject_QueuedNotificationList_2 :: QueuedNotificationList
testObject_QueuedNotificationList_2 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("?",Array [])],fromList [("",Array [Bool False])]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (True) (fmap read (Just "1864-05-16 13:03:49.853403086497 UTC")))
testObject_QueuedNotificationList_3 :: QueuedNotificationList
testObject_QueuedNotificationList_3 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (True) (fmap read (Just "1864-05-12 05:41:37.579202492466 UTC")))
testObject_QueuedNotificationList_4 :: QueuedNotificationList
testObject_QueuedNotificationList_4 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-19 13:49:29.979249907722 UTC")))
testObject_QueuedNotificationList_5 :: QueuedNotificationList
testObject_QueuedNotificationList_5 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [("",Object (fromList []))]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [],fromList [],fromList [],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [],fromList [],fromList [],fromList []]))))]) (False) (fmap read (Just "1864-05-19 14:23:29.57705798545 UTC")))
