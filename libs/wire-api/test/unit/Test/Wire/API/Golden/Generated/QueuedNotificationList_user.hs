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
testObject_QueuedNotificationList_user_1 :: QueuedNotificationList
testObject_QueuedNotificationList_user_1 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (True) (fmap read (Just "1864-05-01 06:55:52.086970485855 UTC")))
testObject_QueuedNotificationList_user_2 :: QueuedNotificationList
testObject_QueuedNotificationList_user_2 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002")))) ((List1 (NonEmpty.fromList [fromList [],fromList [("'/",Null)]]))))]) (True) (fmap read (Just "1864-05-01 20:10:23.405170800557 UTC")))
testObject_QueuedNotificationList_user_3 :: QueuedNotificationList
testObject_QueuedNotificationList_user_3 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("4",Array [Null])]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [("f",Object (fromList [("",Number (0.0))]))],fromList [("\8316",Array [Number (-10.0)])]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("\1100288",Array [String "",String "",Null,Bool False,Number (0.0),String "",String "",Null,String ""])]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("",Null)],fromList [],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [("#",Object (fromList [("",Bool True)]))]]))))]) (False) (fmap read (Nothing)))
testObject_QueuedNotificationList_user_4 :: QueuedNotificationList
testObject_QueuedNotificationList_user_4 = (queuedNotificationList ([]) (False) (fmap read (Just "1864-05-10 05:23:42.099846196659 UTC")))
testObject_QueuedNotificationList_user_5 :: QueuedNotificationList
testObject_QueuedNotificationList_user_5 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000")))) ((List1 (NonEmpty.fromList [fromList [("~\992295",Array [Number (1.0)])],fromList [],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [],fromList []]))))]) (False) (fmap read (Just "1864-05-04 08:18:15.662833139274 UTC")))
testObject_QueuedNotificationList_user_6 :: QueuedNotificationList
testObject_QueuedNotificationList_user_6 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList [("k",Array [String "*t",Bool True])],fromList [],fromList [("Q",Array [])],fromList [],fromList []]))))]) (False) (fmap read (Just "1864-05-05 13:10:36.830828846344 UTC")))
testObject_QueuedNotificationList_user_7 :: QueuedNotificationList
testObject_QueuedNotificationList_user_7 = (queuedNotificationList ([]) (False) (fmap read (Just "1864-04-29 05:48:43.211914220035 UTC")))
testObject_QueuedNotificationList_user_8 :: QueuedNotificationList
testObject_QueuedNotificationList_user_8 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-16 06:50:47.584916592742 UTC")))
testObject_QueuedNotificationList_user_9 :: QueuedNotificationList
testObject_QueuedNotificationList_user_9 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList [],fromList [("",Array [])]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [("\1067471",Object (fromList [("",String "")]))]]))))]) (True) (fmap read (Just "1864-04-29 13:45:07.85379854351 UTC")))
testObject_QueuedNotificationList_user_10 :: QueuedNotificationList
testObject_QueuedNotificationList_user_10 = (queuedNotificationList ([]) (True) (fmap read (Just "1864-05-15 14:41:38.799619470271 UTC")))
testObject_QueuedNotificationList_user_11 :: QueuedNotificationList
testObject_QueuedNotificationList_user_11 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002")))) ((List1 (NonEmpty.fromList [fromList [("\DC3",Array []),("!\SI",Array [])],fromList [("\ETB\SYN",Null)]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002")))) ((List1 (NonEmpty.fromList [fromList [("X\1063936",Object (fromList [])),("b",Array [String "\8504",Bool False])]]))))]) (True) (fmap read (Just "1864-05-09 21:53:38.338193019546 UTC")))
testObject_QueuedNotificationList_user_12 :: QueuedNotificationList
testObject_QueuedNotificationList_user_12 = (queuedNotificationList ([]) (False) (fmap read (Just "1864-05-18 23:20:25.941842549815 UTC")))
testObject_QueuedNotificationList_user_13 :: QueuedNotificationList
testObject_QueuedNotificationList_user_13 = (queuedNotificationList ([]) (False) (fmap read (Nothing)))
testObject_QueuedNotificationList_user_14 :: QueuedNotificationList
testObject_QueuedNotificationList_user_14 = (queuedNotificationList ([]) (False) (fmap read (Just "1864-04-30 20:42:11.686275014057 UTC")))
testObject_QueuedNotificationList_user_15 :: QueuedNotificationList
testObject_QueuedNotificationList_user_15 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Nothing)))
testObject_QueuedNotificationList_user_16 :: QueuedNotificationList
testObject_QueuedNotificationList_user_16 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [("g",Object (fromList []))],fromList [],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (True) (fmap read (Just "1864-05-01 02:16:23.571229153639 UTC")))
testObject_QueuedNotificationList_user_17 :: QueuedNotificationList
testObject_QueuedNotificationList_user_17 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-05 14:01:34.06678099816 UTC")))
testObject_QueuedNotificationList_user_18 :: QueuedNotificationList
testObject_QueuedNotificationList_user_18 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-04-29 04:13:37.395105461358 UTC")))
testObject_QueuedNotificationList_user_19 :: QueuedNotificationList
testObject_QueuedNotificationList_user_19 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-14 08:33:29.525874965464 UTC")))
testObject_QueuedNotificationList_user_20 :: QueuedNotificationList
testObject_QueuedNotificationList_user_20 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-19 19:00:19.936111661227 UTC")))
