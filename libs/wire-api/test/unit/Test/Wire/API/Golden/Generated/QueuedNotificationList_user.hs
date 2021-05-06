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
testObject_QueuedNotificationList_user_1 = (queuedNotificationList ([]) (True) (fmap read (Just "1864-05-01 04:32:23.043671201288 UTC")))
testObject_QueuedNotificationList_user_2 :: QueuedNotificationList
testObject_QueuedNotificationList_user_2 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("",Number (10.0))]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [("v",Object (fromList [("",Number (0.0))]))],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-07 05:26:22.939316706495 UTC")))
testObject_QueuedNotificationList_user_3 :: QueuedNotificationList
testObject_QueuedNotificationList_user_3 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [("`",Array [])]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [],fromList [("",Array [Bool False])]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [(" ",Object (fromList []))],fromList [],fromList [],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [],fromList [],fromList []]))))]) (True) (fmap read (Just "1864-05-01 13:50:37.767408323023 UTC")))
testObject_QueuedNotificationList_user_4 :: QueuedNotificationList
testObject_QueuedNotificationList_user_4 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("{",Array [])]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList [("",Null)],fromList [],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-01 03:11:44.341064016525 UTC")))
testObject_QueuedNotificationList_user_5 :: QueuedNotificationList
testObject_QueuedNotificationList_user_5 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [("6",Bool True)],fromList [],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("",Array [Null])],fromList [],fromList []]))))]) (True) (fmap read (Just "1864-05-11 19:37:08.725460981542 UTC")))
testObject_QueuedNotificationList_user_6 :: QueuedNotificationList
testObject_QueuedNotificationList_user_6 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-09 05:04:51.515655180362 UTC")))
testObject_QueuedNotificationList_user_7 :: QueuedNotificationList
testObject_QueuedNotificationList_user_7 = (queuedNotificationList ([]) (True) (fmap read (Just "1864-05-11 19:42:01.518023028998 UTC")))
testObject_QueuedNotificationList_user_8 :: QueuedNotificationList
testObject_QueuedNotificationList_user_8 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (True) (fmap read (Just "1864-05-07 01:23:25.548690393075 UTC")))
testObject_QueuedNotificationList_user_9 :: QueuedNotificationList
testObject_QueuedNotificationList_user_9 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000")))) ((List1 (NonEmpty.fromList [fromList [("ZO\1032774{6",Number (-4.0))],fromList [("",Array [Null,Bool True,Null])],fromList [("\1065906",Array [])],fromList [("r",Array [Null])],fromList [("&",Object (fromList [("\ESC",Null)]))]]))))]) (False) (fmap read (Just "1864-05-14 09:41:56.783558443567 UTC")))
testObject_QueuedNotificationList_user_10 :: QueuedNotificationList
testObject_QueuedNotificationList_user_10 = (queuedNotificationList ([]) (False) (fmap read (Just "1864-05-16 20:17:52.044650606752 UTC")))
testObject_QueuedNotificationList_user_11 :: QueuedNotificationList
testObject_QueuedNotificationList_user_11 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000")))) ((List1 (NonEmpty.fromList [fromList [],fromList [("",Array [Bool True]),("\156796",Bool True)],fromList []]))))]) (False) (fmap read (Just "1864-05-07 11:21:16.284243379639 UTC")))
testObject_QueuedNotificationList_user_12 :: QueuedNotificationList
testObject_QueuedNotificationList_user_12 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002")))) ((List1 (NonEmpty.fromList [fromList [("",Number (0.0))],fromList [("",Null)],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (True) (fmap read (Just "1864-05-02 23:12:26.255199681894 UTC")))
testObject_QueuedNotificationList_user_13 :: QueuedNotificationList
testObject_QueuedNotificationList_user_13 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (True) (fmap read (Just "1864-05-14 10:24:06.311981569713 UTC")))
testObject_QueuedNotificationList_user_14 :: QueuedNotificationList
testObject_QueuedNotificationList_user_14 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [("",Number (-10.0))]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [],fromList [],fromList []]))))]) (False) (fmap read (Just "1864-04-30 13:35:15.889563736636 UTC")))
testObject_QueuedNotificationList_user_15 :: QueuedNotificationList
testObject_QueuedNotificationList_user_15 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [("4",Array [])],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList [],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("",Object (fromList []))],fromList [("",Object (fromList [("",Number (0.0))]))]]))))]) (True) (fmap read (Just "1864-05-19 08:11:21.762924392271 UTC")))
testObject_QueuedNotificationList_user_16 :: QueuedNotificationList
testObject_QueuedNotificationList_user_16 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList [("K",String ";")],fromList [("",Object (fromList [("",Number (0.0))]))]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList [("S",Array [Bool False,Number (0.0),String ""])]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("",Array [Number (-1.0)])]]))))]) (True) (fmap read (Just "1864-05-14 21:18:43.13116759037 UTC")))
testObject_QueuedNotificationList_user_17 :: QueuedNotificationList
testObject_QueuedNotificationList_user_17 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-07 06:27:31.624430036044 UTC")))
testObject_QueuedNotificationList_user_18 :: QueuedNotificationList
testObject_QueuedNotificationList_user_18 = (queuedNotificationList ([]) (False) (fmap read (Just "1864-05-08 02:51:35.98767050549 UTC")))
testObject_QueuedNotificationList_user_19 :: QueuedNotificationList
testObject_QueuedNotificationList_user_19 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (True) (fmap read (Just "1864-05-14 00:17:45.65935390911 UTC")))
testObject_QueuedNotificationList_user_20 :: QueuedNotificationList
testObject_QueuedNotificationList_user_20 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (True) (fmap read (Nothing)))
