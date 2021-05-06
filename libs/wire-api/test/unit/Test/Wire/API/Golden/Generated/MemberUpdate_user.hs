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
testObject_MemberUpdate_user_1 = MemberUpdate {mupOtrMute = Nothing, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "\140185?", mupOtrArchive = Just True, mupOtrArchiveRef = Just "GAy", mupHidden = Just True, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "6usiabc718t_d35tsviwt"))}
testObject_MemberUpdate_user_2 :: MemberUpdate
testObject_MemberUpdate_user_2 = MemberUpdate {mupOtrMute = Nothing, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "\\\1012983", mupOtrArchive = Nothing, mupOtrArchiveRef = Just "4~", mupHidden = Just True, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "zuigl82tdh_zjg71_io4g4zd64hh10oa6z06hke2pu3fsdhzfv0kvhr3b58mhnnpg7q5dly4xu6qsy1y_oi_ra6y2959fdkmatkhpa91u1g"))}
testObject_MemberUpdate_user_3 :: MemberUpdate
testObject_MemberUpdate_user_3 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "\986022", mupOtrArchive = Nothing, mupOtrArchiveRef = Just "#'", mupHidden = Just True, mupHiddenRef = Just "", mupConvRoleName = Just (fromJust (parseRoleName "5_863hh12nztc_68zi_sp1xxa248xnnr49j7t301r17mrvqw9mq3ptqppmt3faf3_5fb_am5c9kgn7h7i4l0epkr2kn814bjb1vp03f17rcrkcqzoq6c1h"))}
testObject_MemberUpdate_user_4 :: MemberUpdate
testObject_MemberUpdate_user_4 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Nothing, mupOtrArchiveRef = Just "(\b", mupHidden = Just True, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "o1l6xu5kicuwq8umc1lnfacclfx"))}
testObject_MemberUpdate_user_5 :: MemberUpdate
testObject_MemberUpdate_user_5 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Nothing, mupOtrArchiveRef = Just "\17125", mupHidden = Nothing, mupHiddenRef = Just "\1060170\997055G", mupConvRoleName = Just (fromJust (parseRoleName "5g93eghq7sg0hi_rommurx_jj8ipw_crtxs9gqym592uwf2egrya5bgag5ymak6xxrlgzhr9vbjydx_fxcs5s8ab6wmdl1"))}
testObject_MemberUpdate_user_6 :: MemberUpdate
testObject_MemberUpdate_user_6 = MemberUpdate {mupOtrMute = Nothing, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just False, mupOtrArchiveRef = Nothing, mupHidden = Nothing, mupHiddenRef = Just "", mupConvRoleName = Nothing}
testObject_MemberUpdate_user_7 :: MemberUpdate
testObject_MemberUpdate_user_7 = MemberUpdate {mupOtrMute = Nothing, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "\179524", mupOtrArchive = Nothing, mupOtrArchiveRef = Just "%\186365\DC1", mupHidden = Just False, mupHiddenRef = Just "s", mupConvRoleName = Just (fromJust (parseRoleName "bbek6a7foq4i5vnaq5al2qqontbspfobel_qccu7cnkdmlhqpxr0n2m93eqivwww7qemtclh04mknm7ixpkkz0kyo9d79fcbc1m5v"))}
testObject_MemberUpdate_user_8 :: MemberUpdate
testObject_MemberUpdate_user_8 = MemberUpdate {mupOtrMute = Nothing, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Just False, mupOtrArchiveRef = Nothing, mupHidden = Nothing, mupHiddenRef = Nothing, mupConvRoleName = Nothing}
testObject_MemberUpdate_user_9 :: MemberUpdate
testObject_MemberUpdate_user_9 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Just False, mupOtrArchiveRef = Nothing, mupHidden = Just True, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "dgadsqehy98iqh9g6_7ht5oynk6u71thxfjbue3lo8qf10bfabh7y5x"))}
testObject_MemberUpdate_user_10 :: MemberUpdate
testObject_MemberUpdate_user_10 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just False, mupOtrArchiveRef = Just "/", mupHidden = Just True, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "532_0cc1eed222wjo2s7k29x3ile6mqa6"))}
testObject_MemberUpdate_user_11 :: MemberUpdate
testObject_MemberUpdate_user_11 = MemberUpdate {mupOtrMute = Nothing, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just False, mupOtrArchiveRef = Just "a", mupHidden = Nothing, mupHiddenRef = Just "\992145", mupConvRoleName = Just (fromJust (parseRoleName "mtwouik1d1mw1xehtcareswhcfdwsl_7cqjtt94a2s5mb4gmb2ajvb4dfq1ndu3dk"))}
testObject_MemberUpdate_user_12 :: MemberUpdate
testObject_MemberUpdate_user_12 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Just False, mupOtrArchiveRef = Just "2", mupHidden = Nothing, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "h8ugznercozwdkbqfr7rdt6w8ld6w9nuthpqwfyz2j_lypw7qmumr_job2oo4fx8gudz"))}
testObject_MemberUpdate_user_13 :: MemberUpdate
testObject_MemberUpdate_user_13 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "\186070A\DC1", mupOtrArchive = Just True, mupOtrArchiveRef = Just "\1024378:", mupHidden = Just True, mupHiddenRef = Just "", mupConvRoleName = Just (fromJust (parseRoleName "kdr310uv2zi5bmhu6m25d_9ha34w2bmda0d8ucg80wf8cbjt39"))}
testObject_MemberUpdate_user_14 :: MemberUpdate
testObject_MemberUpdate_user_14 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Just True, mupOtrArchiveRef = Just "@,", mupHidden = Just True, mupHiddenRef = Just "[\989507\&9", mupConvRoleName = Just (fromJust (parseRoleName "232sg3megkfgy2icq6lynkusm7bqkmqmv27hkqm2gdkva16h3t2k9nyq75_4d1gc9wl"))}
testObject_MemberUpdate_user_15 :: MemberUpdate
testObject_MemberUpdate_user_15 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just False, mupOtrArchiveRef = Just "", mupHidden = Just True, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "rv2jign5ras9d90b8foj6oi8h4wmiycnsqhgnuw11w_izhd5z252meat066nu99_y2slwpyfl8wkn1_fncjwnllhe"))}
testObject_MemberUpdate_user_16 :: MemberUpdate
testObject_MemberUpdate_user_16 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "H+", mupOtrArchive = Just False, mupOtrArchiveRef = Just "m\te", mupHidden = Just False, mupHiddenRef = Just "4ts", mupConvRoleName = Just (fromJust (parseRoleName "cjyikgxu0p7igzczosb7yj27ynk0_v15myvbmdcl7wzhv8kcyus008doxuor5sgeut13b78p4mtrrmhjgzm9kohzy1fe2miz59enurajqq99gb"))}
testObject_MemberUpdate_user_17 :: MemberUpdate
testObject_MemberUpdate_user_17 = MemberUpdate {mupOtrMute = Just False, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Nothing, mupOtrArchive = Just True, mupOtrArchiveRef = Nothing, mupHidden = Just False, mupHiddenRef = Just "", mupConvRoleName = Nothing}
testObject_MemberUpdate_user_18 :: MemberUpdate
testObject_MemberUpdate_user_18 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "", mupOtrArchive = Just True, mupOtrArchiveRef = Just "\EOT`", mupHidden = Nothing, mupHiddenRef = Just "|", mupConvRoleName = Just (fromJust (parseRoleName "vmae_kyl9tghtgv8r5pcbb6mszrypgfffy_tocyi2gwzjmcvpm4dmwcid2e2zjyhxm7f67x_7yit3vaa74oixb42_d2ruf_xu468jvsyzslx1j8o26gxt"))}
testObject_MemberUpdate_user_19 :: MemberUpdate
testObject_MemberUpdate_user_19 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "\ETB/", mupOtrArchive = Nothing, mupOtrArchiveRef = Just "sn", mupHidden = Just False, mupHiddenRef = Nothing, mupConvRoleName = Just (fromJust (parseRoleName "jqkyhjo_k84zqes9f30ou8w714scoj4oi35qty"))}
testObject_MemberUpdate_user_20 :: MemberUpdate
testObject_MemberUpdate_user_20 = MemberUpdate {mupOtrMute = Just True, mupOtrMuteStatus = Nothing, mupOtrMuteRef = Just "Y", mupOtrArchive = Just False, mupOtrArchiveRef = Just "\f\ENQ", mupHidden = Just True, mupHiddenRef = Just "X\1027682", mupConvRoleName = Just (fromJust (parseRoleName "e9kak1_elxw12gf433rvx70a52kcsz_go1i2g57qlkpws4g1etduc6drt8k3ojo"))}
