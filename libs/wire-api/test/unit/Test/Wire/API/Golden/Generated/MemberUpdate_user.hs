{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.MemberUpdate_user where

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
testObject_MemberUpdate_user_1 :: MemberUpdate
testObject_MemberUpdate_user_1 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "\CAN\1105978", mupOtrArchive = Just False, mupOtrArchiveRef = Just "\1110992X", mupHidden = Just False, mupHiddenRef = Just "\EM,", mupConvRoleName = Nothing}
testObject_MemberUpdate_user_2 :: MemberUpdate
testObject_MemberUpdate_user_2 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Just False, mupOtrArchiveRef = Just "\EOT\\", mupHidden = Just True, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "ywrl7agks8hso9qns_ogyyc8hwt0aqc0nzdldfh425mp3geka7muhhh4att07ejeg40s49z77o7p6e_qsqg1h2vyxstbue09826t2pg5g1ta9e2"))}
testObject_MemberUpdate_user_3 :: MemberUpdate
testObject_MemberUpdate_user_3 = MemberUpdate {mupOtrMute = Nothing, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Nothing, mupOtrArchiveRef = Just "\1026454U", mupHidden = Nothing, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "2w2wwf3tiuxf1bqim08whsixf4_23aq2xhb85cb5rid7xn9d4sx_1o"))}
testObject_MemberUpdate_user_4 :: MemberUpdate
testObject_MemberUpdate_user_4 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "\1055435\EMS", mupOtrArchive = Just False, mupOtrArchiveRef = Just "W\US\135038", mupHidden = Just True, mupHiddenRef = Just "", mupConvRoleName = Nothing}
testObject_MemberUpdate_user_5 :: MemberUpdate
testObject_MemberUpdate_user_5 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "4", mupOtrArchive = Just True, mupOtrArchiveRef = Just "\15575K", mupHidden = Just True, mupHiddenRef = Just "\180231\b|", mupConvRoleName = Just (fromJust (parseRoleName "1u6ber2yczio8rg7thlv7snumonhjgoybp0povgiauq3secjwcf2ghw19313nk7j_dseyq0xgcm6voc7g9v54hamto5fvsy7n7xp0q_d8gf19o9m2y64mblqyr1"))}
