{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Member_user where

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
testObject_Member_1 :: Member
testObject_Member_1 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "M\191433", memOtrArchived = True, memOtrArchivedRef = Just "JTK", memHidden = False, memHiddenRef = Just "=\RS\EM", memConvRoleName = (fromJust (parseRoleName "56r0ofp"))}
testObject_Member_2 :: Member
testObject_Member_2 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Just "T\186891", memHidden = True, memHiddenRef = Just "\135473", memConvRoleName = (fromJust (parseRoleName "qeduj4gry1k716wwg28fj0cfci"))}
testObject_Member_3 :: Member
testObject_Member_3 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "\SOH\131858\STX", memConvRoleName = (fromJust (parseRoleName "6e0zcbw4ltuxoe7iq6_li1q6brl8_bv45iiwoqssna73hh_y9ms39z4l2xcmht71l61g5jj1z3de8f9ii7imif"))}
testObject_Member_4 :: Member
testObject_Member_4 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "\1032152\DEL0", memOtrArchived = True, memOtrArchivedRef = Just "7", memHidden = False, memHiddenRef = Just "\150955", memConvRoleName = (fromJust (parseRoleName "i6prv7i8rc6l8biew4bbtb2oaluteie0duj02zp_brf"))}
testObject_Member_5 :: Member
testObject_Member_5 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "n4322t7zq5q9g93rz8sou02b24_lzatuojflw8rfubh0fj7an5tf3_j3grm0ctsveza69j7d_a_47lj5itugva"))}
