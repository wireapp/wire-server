{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.OtherMemberUpdate_user where

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
testObject_OtherMemberUpdate_user_1 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_1 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "t7z_z68n7b7thiayxbmw523xb_2wbni32utng_985b9225cqjqy_a5k7sqbfbveh3b348ucyha0u6w259igcalpeh04b"))}
testObject_OtherMemberUpdate_user_2 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_2 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "rp63f6xbvh21f0grnc2f7m7bnx_vd4y4e2y7v5w5kvp4x5h2m0mgz0bfc97dnmgtggf_tbfykos27d81hdadkfspm1g27glto8hfrna3_at1bgic3fkkozh63w3"))}
testObject_OtherMemberUpdate_user_3 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_3 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "u22yardsm9y97mw37fby4l59izazdis8ycksz9k53vadzodk"))}
testObject_OtherMemberUpdate_user_4 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_4 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "sk331kneewl_det_kry35czdqzue42_hvp"))}
testObject_OtherMemberUpdate_user_5 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_5 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "8ndq8ii18ve9xeodnwe52ftxgo2j6crtzsq8ypwfk5ujhasosnc3vejwze0ez1kjje1r7lmy81hnx9z"))}
