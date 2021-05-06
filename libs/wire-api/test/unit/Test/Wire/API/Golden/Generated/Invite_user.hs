{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Invite_user where

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
testObject_Invite_user_1 :: Invite
testObject_Invite_user_1 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000066-0000-003a-0000-001e0000005f"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))),(Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))),(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))])), invRoleName = (fromJust (parseRoleName "p_7m5k1n5gxtgzsqi8n9xkb9vele0oickkqgler"))}
testObject_Invite_user_2 :: Invite
testObject_Invite_user_2 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000025-0000-000b-0000-007500000027")))])), invRoleName = (fromJust (parseRoleName "bd8vj18_5pv8murvw84v048_57p3b3n4js28p680u61a5b6dqbc72smdblae__p9ygd6e7aes6p9gcpq9zky7f0"))}
testObject_Invite_user_3 :: Invite
testObject_Invite_user_3 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000050-0000-0031-0000-000e0000004a"))),(Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000200000006"))),(Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000800000001")))])), invRoleName = (fromJust (parseRoleName "04xtuioor2d"))}
testObject_Invite_user_4 :: Invite
testObject_Invite_user_4 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000068-0000-0080-0000-005d00000032")))])), invRoleName = (fromJust (parseRoleName "ymq95l1yyh5"))}
testObject_Invite_user_5 :: Invite
testObject_Invite_user_5 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000026-0000-001c-0000-007f00000051"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))])), invRoleName = (fromJust (parseRoleName "5xf2_pz3ek07fk5oz256o9q2edo_yz1sgg0aviaz1w2s19ol9f9robvksimknc448rzn2ptgt3af"))}
