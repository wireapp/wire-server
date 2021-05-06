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
testObject_Member_user_1 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), memOtrMutedRef = Just "K", memOtrArchived = True, memOtrArchivedRef = Just "=", memHidden = True, memHiddenRef = Just "J", memConvRoleName = (fromJust (parseRoleName "hyuojkcpjqe3b9e1co7vx991e8vcfccn0cx8yiv"))}
testObject_Member_user_2 :: Member
testObject_Member_user_2 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Nothing, memOtrArchived = True, memOtrArchivedRef = Just "\NUL\148308 ", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "uryzgvy_w73aa7p_4nfrqucj55hxz426703mqs6eznsxt87v4sd33ng11y3r0sxc_tpij96qxga_n1r88slzfta7tz6l8"))}
testObject_Member_user_3 :: Member
testObject_Member_user_3 = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "|J", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "\183625", memConvRoleName = (fromJust (parseRoleName "dpo8iwf6veuawdsh96f361jf_wi7l3r357fzilvw1ltcz0m_smvfci9i2g_ihc0kwvkjk"))}
testObject_Member_user_4 :: Member
testObject_Member_user_4 = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "c ", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Just "\DC31\SUB", memConvRoleName = (fromJust (parseRoleName "hur6ob"))}
testObject_Member_user_5 :: Member
testObject_Member_user_5 = Member {memId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "n", memHidden = True, memHiddenRef = Just "#", memConvRoleName = (fromJust (parseRoleName "ge1s8gie9o1ko3upibdifngh5v25"))}
