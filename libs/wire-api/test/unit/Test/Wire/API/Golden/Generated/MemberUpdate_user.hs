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
testObject_MemberUpdate_user_1 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "\FSR\EOT", mupOtrArchive = Just False, mupOtrArchiveRef = Just "#", mupHidden = Nothing, mupHiddenRef = Just "\1029762", mupConvRoleName = Nothing}
testObject_MemberUpdate_user_2 :: MemberUpdate
testObject_MemberUpdate_user_2 = MemberUpdate {mupOtrMute = Nothing, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "8", mupOtrArchive = Just True, mupOtrArchiveRef = Just "\fD", mupHidden = Just False, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "otkjrnp5iq6"))}
testObject_MemberUpdate_user_3 :: MemberUpdate
testObject_MemberUpdate_user_3 = MemberUpdate {mupOtrMute = Nothing, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just ")\ESC", mupOtrArchive = Just False, mupOtrArchiveRef = Just "#", mupHidden = Just False, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "qpte330m7fewq1gb4d7vo4x49cxbgsktnai7vn32hgoi_lq_55bdhtmn1jd_ytykk6kkwv2oyabkphi2qr0z2so8eha6ptiwl9"))}
testObject_MemberUpdate_user_4 :: MemberUpdate
testObject_MemberUpdate_user_4 = MemberUpdate {mupOtrMute = Nothing, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "X=_", mupOtrArchive = Just True, mupOtrArchiveRef = Just "=", mupHidden = Just False, mupHiddenRef = Just "\94255+", mupConvRoleName = Just (fromJust (parseRoleName "lrvwyzm8etkb03er0rlua1feepu5z9ouqyqsezuf1hxlhmhwm7qtsgvh_39j0_7y405ca6vl9pa4dgnccp33fpgeygv8enpwmo_hs6_vqltldra3lgxl6lbulo"))}
testObject_MemberUpdate_user_5 :: MemberUpdate
testObject_MemberUpdate_user_5 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Just True, mupOtrArchiveRef = Just "\ESC\988895\158548", mupHidden = Nothing, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "lb3laa580hqib_2sr1"))}
testObject_MemberUpdate_user_6 :: MemberUpdate
testObject_MemberUpdate_user_6 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "\163933", mupOtrArchive = Just False, mupOtrArchiveRef = Just "}\r\1025813", mupHidden = Nothing, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "37bl644f6h4uldaz_4br"))}
testObject_MemberUpdate_user_7 :: MemberUpdate
testObject_MemberUpdate_user_7 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "ipX", mupOtrArchive = Nothing, mupOtrArchiveRef = Just "<\985764c", mupHidden = Just False, mupHiddenRef = Just "}\147798e", mupConvRoleName = Just (fromJust (parseRoleName "_dd9upsm6cl_jblhx_wttzmtrgu8lm3i8k_uzfejv4sxsav0e5mp1cnm2m56s19fjeuw1crdqony534uzdjuzkizht1eii52vjwnlhu6ifm5gclt"))}
testObject_MemberUpdate_user_8 :: MemberUpdate
testObject_MemberUpdate_user_8 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "_7", mupOtrArchive = Nothing, mupOtrArchiveRef = Just "\48931", mupHidden = Just True, mupHiddenRef = Just "", mupConvRoleName = Just (fromJust (parseRoleName "hcs_8ol446md2g70po5kl5vnws4_ux9rbh91s6va57p8kc8k_nkjprqkk3jmggh2f3i_kf0oun27rbcy6rmkrxnlpogf"))}
testObject_MemberUpdate_user_9 :: MemberUpdate
testObject_MemberUpdate_user_9 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Nothing, mupOtrArchiveRef = Just "Z", mupHidden = Just True, mupHiddenRef = Just "\ETBa", mupConvRoleName = Just (fromJust (parseRoleName "x91l73_2ho7s0t4frtlpx5jwjpu_eum2ftkh9jf2tmcrzzcuq51baqfl25o1x441u0cbc"))}
testObject_MemberUpdate_user_10 :: MemberUpdate
testObject_MemberUpdate_user_10 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "\1067008", mupOtrArchive = Nothing, mupOtrArchiveRef = Just "", mupHidden = Just True, mupHiddenRef = Just "(\60866", mupConvRoleName = Just (fromJust (parseRoleName "_1e7fssvdc3tt_qpll3zvt1s98jbn5daknh2qoex7j4v0hrhkty_17yup21qo5ndjrpxdmf7nijfaxiyq4b1ltdn797ijfo3y91uu2fjrc1gg0825fg2rhi3z"))}
testObject_MemberUpdate_user_11 :: MemberUpdate
testObject_MemberUpdate_user_11 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just True, mupOtrArchiveRef = Nothing, mupHidden = Just True, mupHiddenRef = Just "", mupConvRoleName = Just (fromJust (parseRoleName "5lu7qrmplsj06jjwatym07eaobr8m6sb2gu0yu51mqbag3de0k_861yh1ayj1v_0qc25i44xyne4qsj4b0fqc9kietq932wj91u1ohv"))}
testObject_MemberUpdate_user_12 :: MemberUpdate
testObject_MemberUpdate_user_12 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "]%", mupOtrArchive = Just False, mupOtrArchiveRef = Just "O", mupHidden = Nothing, mupHiddenRef = Just "\1006122+", mupConvRoleName = Just (fromJust (parseRoleName "56srxg97rk5ruql_ea0jgp97uy394ouc4pvkaj4v3gtcmmy9_bmd0cgfiq61"))}
testObject_MemberUpdate_user_13 :: MemberUpdate
testObject_MemberUpdate_user_13 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Just True, mupOtrArchiveRef = Just "", mupHidden = Just True, mupHiddenRef = Just "\1088869.", mupConvRoleName = Nothing}
testObject_MemberUpdate_user_14 :: MemberUpdate
testObject_MemberUpdate_user_14 = MemberUpdate {mupOtrMute = Nothing, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "g\SOH\1075136", mupOtrArchive = Just True, mupOtrArchiveRef = Just "<.", mupHidden = Just True, mupHiddenRef = Just "\SYN", mupConvRoleName = Nothing}
testObject_MemberUpdate_user_15 :: MemberUpdate
testObject_MemberUpdate_user_15 = MemberUpdate {mupOtrMute = Nothing, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just False, mupOtrArchiveRef = Nothing, mupHidden = Just False, mupHiddenRef = Just "", mupConvRoleName = Just (fromJust (parseRoleName "oowy4kgqcul89uqne7stz2nz7s23v8d0bwk7l8u8u3znir_1j5_7qdll"))}
testObject_MemberUpdate_user_16 :: MemberUpdate
testObject_MemberUpdate_user_16 = MemberUpdate {mupOtrMute = Nothing, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Nothing, mupOtrArchiveRef = Nothing, mupHidden = Just True, mupHiddenRef = Just "\153435\&4\\", mupConvRoleName = Nothing}
testObject_MemberUpdate_user_17 :: MemberUpdate
testObject_MemberUpdate_user_17 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just False, mupOtrArchiveRef = Nothing, mupHidden = Just False, mupHiddenRef = Just "", mupConvRoleName = Nothing}
testObject_MemberUpdate_user_18 :: MemberUpdate
testObject_MemberUpdate_user_18 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just True, mupOtrArchiveRef = Just "", mupHidden = Nothing, mupHiddenRef = Just "", mupConvRoleName = Just (fromJust (parseRoleName "uyfurhy7quc72sjbkbe37140u97ze8euf0njqr_7pn5gfgwfhgw3xhrbum4mhz8pm7sjamg998wdvi8co6doc3le97_2k31cqs9w4v3rgyze7"))}
testObject_MemberUpdate_user_19 :: MemberUpdate
testObject_MemberUpdate_user_19 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "X", mupOtrArchive = Just True, mupOtrArchiveRef = Just "N", mupHidden = Nothing, mupHiddenRef = Just "\58863\142737{", mupConvRoleName = Just (fromJust (parseRoleName "loemeuyn"))}
testObject_MemberUpdate_user_20 :: MemberUpdate
testObject_MemberUpdate_user_20 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just True, mupOtrArchiveRef = Just "", mupHidden = Just False, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "ljn593jhcza806ess24hm2a91066biyf7memotae9rwq58x2jvnickgbc4_i6bn__cfzgafn24owvorob5ppvqzg_p4a2xkmdohr"))}
