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
testObject_QueuedNotificationList_user_1 = (queuedNotificationList ([]) (True) (fmap read (Just "1864-05-03 08:06:58.671844608095 UTC")))
testObject_QueuedNotificationList_user_2 :: QueuedNotificationList
testObject_QueuedNotificationList_user_2 = (queuedNotificationList ([]) (False) (fmap read (Just "1864-05-01 11:14:29.042976814118 UTC")))
testObject_QueuedNotificationList_user_3 :: QueuedNotificationList
testObject_QueuedNotificationList_user_3 = (queuedNotificationList ([]) (False) (fmap read (Just "1864-05-03 14:00:37.435552342103 UTC")))
testObject_QueuedNotificationList_user_4 :: QueuedNotificationList
testObject_QueuedNotificationList_user_4 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [("",String "M")]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [],fromList [("",Null)]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [],fromList [],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-14 14:15:39.087166213546 UTC")))
testObject_QueuedNotificationList_user_5 :: QueuedNotificationList
testObject_QueuedNotificationList_user_5 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (True) (fmap read (Nothing)))
testObject_QueuedNotificationList_user_6 :: QueuedNotificationList
testObject_QueuedNotificationList_user_6 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [("",Object (fromList []))]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList [],fromList [("\1013661",Object (fromList []))]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList [],fromList [],fromList [],fromList [],fromList [],fromList [],fromList [],fromList []]))))]) (False) (fmap read (Just "1864-05-17 18:09:06.33233256614 UTC")))
testObject_QueuedNotificationList_user_7 :: QueuedNotificationList
testObject_QueuedNotificationList_user_7 = (queuedNotificationList ([]) (False) (fmap read (Just "1864-05-18 11:23:17.374581355383 UTC")))
testObject_QueuedNotificationList_user_8 :: QueuedNotificationList
testObject_QueuedNotificationList_user_8 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList [("\DEL\EOT",Null)],fromList [],fromList [("",Object (fromList [("\1094857",Number (0.0))]))]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002")))) ((List1 (NonEmpty.fromList [fromList [("\FS",Object (fromList [("",Number (20.0))]))],fromList [],fromList [],fromList [],fromList [],fromList [],fromList [],fromList [],fromList [],fromList []]))))]) (True) (fmap read (Nothing)))
testObject_QueuedNotificationList_user_9 :: QueuedNotificationList
testObject_QueuedNotificationList_user_9 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("",Object (fromList []))],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-04-30 13:14:32.338935880477 UTC")))
testObject_QueuedNotificationList_user_10 :: QueuedNotificationList
testObject_QueuedNotificationList_user_10 = (queuedNotificationList ([]) (False) (fmap read (Just "1864-04-29 16:48:47.015700709845 UTC")))
testObject_QueuedNotificationList_user_11 :: QueuedNotificationList
testObject_QueuedNotificationList_user_11 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (True) (fmap read (Just "1864-05-16 15:03:13.232700463566 UTC")))
testObject_QueuedNotificationList_user_12 :: QueuedNotificationList
testObject_QueuedNotificationList_user_12 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList [],fromList [("",Array [])]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("M",String "")],fromList [("",Array [])]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList [("",Object (fromList []))]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("",Null)],fromList [("]",Array [])]]))))]) (True) (fmap read (Just "1864-05-14 23:27:26.49027785132 UTC")))
testObject_QueuedNotificationList_user_13 :: QueuedNotificationList
testObject_QueuedNotificationList_user_13 = (queuedNotificationList ([]) (False) (fmap read (Just "1864-05-06 12:17:34.305553103687 UTC")))
testObject_QueuedNotificationList_user_14 :: QueuedNotificationList
testObject_QueuedNotificationList_user_14 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("",Array [Bool False,Null])]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("",Object (fromList []))]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-01 07:25:45.544148203516 UTC")))
testObject_QueuedNotificationList_user_15 :: QueuedNotificationList
testObject_QueuedNotificationList_user_15 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList [("z",Bool False),("\189948",Object (fromList [("",Bool False)]))]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002")))) ((List1 (NonEmpty.fromList [fromList [],fromList [("\STX",Object (fromList []))]]))))]) (False) (fmap read (Just "1864-05-03 00:43:28.889632483624 UTC")))
testObject_QueuedNotificationList_user_16 :: QueuedNotificationList
testObject_QueuedNotificationList_user_16 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002")))) ((List1 (NonEmpty.fromList [fromList [],fromList [("R\ENQ",Array [Bool False]),("",Array [Null])]])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Nothing)))
testObject_QueuedNotificationList_user_17 :: QueuedNotificationList
testObject_QueuedNotificationList_user_17 = (queuedNotificationList ([]) (True) (fmap read (Just "1864-05-17 16:59:11.260103997061 UTC")))
testObject_QueuedNotificationList_user_18 :: QueuedNotificationList
testObject_QueuedNotificationList_user_18 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002")))) ((List1 (NonEmpty.fromList [fromList [("M\NAK",Null),("",Number (0.0))],fromList [],fromList [],fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList [("x",Array [Bool False])],fromList [],fromList [],fromList []]))))]) (True) (fmap read (Just "1864-04-29 03:04:28.917689397243 UTC")))
testObject_QueuedNotificationList_user_19 :: QueuedNotificationList
testObject_QueuedNotificationList_user_19 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-19 02:46:37.257972012352 UTC")))
testObject_QueuedNotificationList_user_20 :: QueuedNotificationList
testObject_QueuedNotificationList_user_20 = (queuedNotificationList ([(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) ((List1 (NonEmpty.fromList [fromList []])))),(queuedNotification ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))) ((List1 (NonEmpty.fromList [fromList []]))))]) (False) (fmap read (Just "1864-05-04 08:05:18.006860895977 UTC")))
