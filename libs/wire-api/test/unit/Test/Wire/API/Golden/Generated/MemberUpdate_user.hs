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
testObject_MemberUpdate_user_1 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Just (MutedStatus {fromMutedStatus = 0}), mupOtrMuteRef = Nothing, mupOtrArchive = Just False, mupOtrArchiveRef = Nothing, mupHidden = Just False, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "cn0umd1jl9uf41b04nzm_jnhy6nmphlax7dk6j3ndzxhzl3x84q372ww973h8_32zoriqv8voe"))}
testObject_MemberUpdate_user_2 :: MemberUpdate
testObject_MemberUpdate_user_2 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Just (MutedStatus {fromMutedStatus = 1}), mupOtrMuteRef = Just "'", mupOtrArchive = Just True, mupOtrArchiveRef = Just "&\\\160503", mupHidden = Just False, mupHiddenRef = Just "J\US\167155", mupConvRoleName = Just (fromJust (parseRoleName "4qjkvrohwnjnoiwcdeytszg51fnss5wkq7l7e9s798xh_q40g1aziz6vwemk5zaxc_uxhy56nqptycrkdatcq36q453xfduj5zi7dmwwh9fsah5e6u873m60on2w"))}
testObject_MemberUpdate_user_3 :: MemberUpdate
testObject_MemberUpdate_user_3 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Just (MutedStatus {fromMutedStatus = -2}), mupOtrMuteRef = Just "\US", mupOtrArchive = Just True, mupOtrArchiveRef = Just "q", mupHidden = Just True, mupHiddenRef = Nothing, mupConvRoleName = Nothing}
testObject_MemberUpdate_user_4 :: MemberUpdate
testObject_MemberUpdate_user_4 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Just (MutedStatus {fromMutedStatus = -1}), mupOtrMuteRef = Nothing, mupOtrArchive = Nothing, mupOtrArchiveRef = Just "", mupHidden = Just False, mupHiddenRef = Just "*\133079M", mupConvRoleName = Just (fromJust (parseRoleName "o3kde9cwrkztkikd0j_2eu7fx184ssv_1lr_x0kfs0a2gpv96e_p9k9g2y"))}
testObject_MemberUpdate_user_5 :: MemberUpdate
testObject_MemberUpdate_user_5 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Just (MutedStatus {fromMutedStatus = 0}), mupOtrMuteRef = Just "", mupOtrArchive = Just False, mupOtrArchiveRef = Just "", mupHidden = Just False, mupHiddenRef = Just "", mupConvRoleName = Just (fromJust (parseRoleName "lnnqtixwpboizqvavxza9b84sv7k0vqk4zm0xynex4qhacur32eroh8ukvucjnpudqb3rnq0_3ez8e7xpsqb0bae8aohpyifim6hlsjrf2w8xuhrsp_oaxilelcs"))}
