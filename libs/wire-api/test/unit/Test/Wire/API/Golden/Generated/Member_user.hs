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
testObject_Member_user_1 :: Member
testObject_Member_user_1 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "2", memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = True, memHiddenRef = Just "\166892f\1073444", memConvRoleName = (fromJust (parseRoleName "ms9ks4jvhg5phz9nnwi_2wyans8h6esfbtk5urdov6f3dmmhvsm99a0nkjy2cy_0v_n6l_vjib2depufo1bzgrjr4jp0n9ty1mq8nlb0ogn02o"))}
testObject_Member_user_2 :: Member
testObject_Member_user_2 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "?\146853", memOtrArchived = False, memOtrArchivedRef = Just "\1036718\SYNT", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "ucjylh7w5hor90j1tf"))}
testObject_Member_user_3 :: Member
testObject_Member_user_3 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Just "\1086152\EOT", memOtrArchived = False, memOtrArchivedRef = Just "O\SO", memHidden = True, memHiddenRef = Just "\58416\139148\11814", memConvRoleName = (fromJust (parseRoleName "mvfhs3zoqs1n8fdh9jlwax9s4d6xdbqanfw2r_117kd_m"))}
testObject_Member_user_4 :: Member
testObject_Member_user_4 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Nothing, memOtrArchived = False, memOtrArchivedRef = Just "e", memHidden = True, memHiddenRef = Just "t", memConvRoleName = (fromJust (parseRoleName "3_u1eoccsjhhxm3c2fodyi1ge3fkrxah5_h6gb6nufairksgbple7ykwhum50qd_iyb_unheu7m7a44flmucxb5165o8277ousbiojv3s_xazv9d"))}
testObject_Member_user_5 :: Member
testObject_Member_user_5 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), memOtrMutedRef = Nothing, memOtrArchived = False, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "\t", memConvRoleName = (fromJust (parseRoleName "2spyw_1mobqhr5bwnvbu6p6wmzmwd8yinpzyc33ckhqlk"))}
