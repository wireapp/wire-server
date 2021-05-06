{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.MemberUpdateData_user where

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
testObject_MemberUpdateData_user_1 :: MemberUpdateData
testObject_MemberUpdateData_user_1 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Nothing, misOtrArchived = Just True, misOtrArchivedRef = Just "\62391\CAN", misHidden = Just True, misHiddenRef = Just "}\EMI", misConvRoleName = Just (fromJust (parseRoleName "gghfuzyr785bf3i0lgx0q_fcc41y492f7axrg3l9j0xq1mp"))}
testObject_MemberUpdateData_user_2 :: MemberUpdateData
testObject_MemberUpdateData_user_2 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Just "7\NAK", misOtrArchived = Just True, misOtrArchivedRef = Just "\\y", misHidden = Just True, misHiddenRef = Just "", misConvRoleName = Just (fromJust (parseRoleName "0r6apdubwpmbhujrnjnf97ym5kkh5vfcg3xvrps8269azd"))}
testObject_MemberUpdateData_user_3 :: MemberUpdateData
testObject_MemberUpdateData_user_3 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Just "", misOtrArchived = Just False, misOtrArchivedRef = Just "F\1004795\ETB", misHidden = Just True, misHiddenRef = Just "", misConvRoleName = Just (fromJust (parseRoleName "z_06yoil4wgqjfknxi9vfnk6gucb5a54ygotwfw2tu96w09wqzc33t_htjynns4mw62ybs7muxklgsfnrwegdax8p8yzbmi2v4twy6jsvmj991xr965zinb8lc"))}
testObject_MemberUpdateData_user_4 :: MemberUpdateData
testObject_MemberUpdateData_user_4 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Just "", misOtrArchived = Just False, misOtrArchivedRef = Just "\7126_", misHidden = Nothing, misHiddenRef = Just "!\132880", misConvRoleName = Just (fromJust (parseRoleName "6ybq7e6ub9b5ftw70ydwgq4zy03p1x_8h6g3nxeci0tj9h4pdcxzzl48bme_z6sxi157h7qselcmd26o8sb9tanr4h9b3u7q7bjqv4qk9dqacckp4f4kz6a4eo5v7d"))}
testObject_MemberUpdateData_user_5 :: MemberUpdateData
testObject_MemberUpdateData_user_5 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Nothing, misOtrArchived = Just True, misOtrArchivedRef = Just "\1046939\GS", misHidden = Nothing, misHiddenRef = Just "p\ESC", misConvRoleName = Just (fromJust (parseRoleName "5zc2oxjo6lwxjhjngcs7xix6qo2"))}
testObject_MemberUpdateData_user_6 :: MemberUpdateData
testObject_MemberUpdateData_user_6 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Nothing, misOtrArchived = Just True, misOtrArchivedRef = Nothing, misHidden = Just True, misHiddenRef = Just "%w", misConvRoleName = Nothing}
testObject_MemberUpdateData_user_7 :: MemberUpdateData
testObject_MemberUpdateData_user_7 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Nothing, misOtrArchived = Just False, misOtrArchivedRef = Nothing, misHidden = Just True, misHiddenRef = Just "c\30463", misConvRoleName = Just (fromJust (parseRoleName "k5ly1eu93413fdn_00o_p0ga9lcj0i8agr_111havylmxiykkt_jma662pwuc87p_bikdin_zcknjqty1w3z6tmmhx123w5nmqbs03hruiz4biwhq35"))}
testObject_MemberUpdateData_user_8 :: MemberUpdateData
testObject_MemberUpdateData_user_8 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), misOtrMuted = Just False, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "\1098519\EM-", misOtrArchived = Just False, misOtrArchivedRef = Nothing, misHidden = Just True, misHiddenRef = Nothing, misConvRoleName = Nothing}
testObject_MemberUpdateData_user_9 :: MemberUpdateData
testObject_MemberUpdateData_user_9 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Just "k>", misOtrArchived = Just False, misOtrArchivedRef = Nothing, misHidden = Nothing, misHiddenRef = Just "\SO", misConvRoleName = Just (fromJust (parseRoleName "m3n99l9u3mssgcvmqgsiobv_a6j98edbyzozy3r96crvxhujd9nhqhj7bjmmzb9b"))}
testObject_MemberUpdateData_user_10 :: MemberUpdateData
testObject_MemberUpdateData_user_10 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), misOtrMuted = Nothing, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "sb", misOtrArchived = Just False, misOtrArchivedRef = Just "\187955a", misHidden = Just True, misHiddenRef = Just "tt", misConvRoleName = Just (fromJust (parseRoleName "tco7vdjqb7svu6rf1rhe0hbfaszgg3e6ggbmfctohw4j3f_zmq1_w274ldgp3itlcoczg9adduvzcwkiy0d8v1hk4ojs6n92"))}
testObject_MemberUpdateData_user_11 :: MemberUpdateData
testObject_MemberUpdateData_user_11 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), misOtrMutedRef = Just "", misOtrArchived = Just False, misOtrArchivedRef = Just "", misHidden = Just False, misHiddenRef = Just "\DLE\DC3", misConvRoleName = Just (fromJust (parseRoleName "4k2wbp8pth4zkzflg608nvpqkwzvtc8gmnc23it1_k"))}
testObject_MemberUpdateData_user_12 :: MemberUpdateData
testObject_MemberUpdateData_user_12 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Just "", misOtrArchived = Nothing, misOtrArchivedRef = Just "\1104171", misHidden = Nothing, misHiddenRef = Just "2G", misConvRoleName = Nothing}
testObject_MemberUpdateData_user_13 :: MemberUpdateData
testObject_MemberUpdateData_user_13 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), misOtrMutedRef = Just "x]", misOtrArchived = Just False, misOtrArchivedRef = Just "", misHidden = Nothing, misHiddenRef = Just "\166400\23996x", misConvRoleName = Just (fromJust (parseRoleName "82k50827c0podnn_mlm_3iv2fr4_phjt82e0f4"))}
testObject_MemberUpdateData_user_14 :: MemberUpdateData
testObject_MemberUpdateData_user_14 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 2}), misOtrMutedRef = Nothing, misOtrArchived = Just True, misOtrArchivedRef = Nothing, misHidden = Just False, misHiddenRef = Just "", misConvRoleName = Nothing}
testObject_MemberUpdateData_user_15 :: MemberUpdateData
testObject_MemberUpdateData_user_15 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Nothing, misOtrArchived = Nothing, misOtrArchivedRef = Just "{%", misHidden = Nothing, misHiddenRef = Just "4", misConvRoleName = Just (fromJust (parseRoleName "_yqydr5nyilrg735x_jg358h5q80q69v6z63aa46ubtp9y9hrh4fdohgc308kz4bj_zz_nrn2mkj0pios7d"))}
testObject_MemberUpdateData_user_16 :: MemberUpdateData
testObject_MemberUpdateData_user_16 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just False, misOtrMutedStatus = Nothing, misOtrMutedRef = Nothing, misOtrArchived = Just False, misOtrArchivedRef = Nothing, misHidden = Nothing, misHiddenRef = Just "Xxv", misConvRoleName = Just (fromJust (parseRoleName "8dpaa29zateek3hb6ybyzu1dslrc6ccv9w7qld5k51qdq9jg70lzl4sv6tgoz2aukagt1y2rlrnk36q8jioep51gvts4ohnmf1qlx3v36l6p5jm9n_"))}
testObject_MemberUpdateData_user_17 :: MemberUpdateData
testObject_MemberUpdateData_user_17 = MemberUpdateData {misTarget = Nothing, misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}), misOtrMutedRef = Just "%\1060328P", misOtrArchived = Just True, misOtrArchivedRef = Just ".", misHidden = Just False, misHiddenRef = Just "=a\1029813", misConvRoleName = Just (fromJust (parseRoleName "wulrd0fd_53ddtdcu82j6qb1pgu5ge6dx4t9__lgmk6gumi0bfi9f4okwn4uls4ot4oh9cr0fe5ffuz"))}
testObject_MemberUpdateData_user_18 :: MemberUpdateData
testObject_MemberUpdateData_user_18 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), misOtrMuted = Just True, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "\\\10461", misOtrArchived = Just True, misOtrArchivedRef = Nothing, misHidden = Just False, misHiddenRef = Just "\58831", misConvRoleName = Just (fromJust (parseRoleName "t0sijzih4g23guvqx8qg8okpf0xc3wwdyk3036m5jzo13bwp"))}
testObject_MemberUpdateData_user_19 :: MemberUpdateData
testObject_MemberUpdateData_user_19 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), misOtrMuted = Just False, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "", misOtrArchived = Nothing, misOtrArchivedRef = Just "{U", misHidden = Just False, misHiddenRef = Just "\20655", misConvRoleName = Just (fromJust (parseRoleName "wotko1"))}
testObject_MemberUpdateData_user_20 :: MemberUpdateData
testObject_MemberUpdateData_user_20 = MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Nothing, misOtrArchived = Just False, misOtrArchivedRef = Nothing, misHidden = Nothing, misHiddenRef = Just "", misConvRoleName = Nothing}
