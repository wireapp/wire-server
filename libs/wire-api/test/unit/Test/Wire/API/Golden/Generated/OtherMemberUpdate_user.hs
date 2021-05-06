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
testObject_OtherMemberUpdate_user_1 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "jkasd_0p2bceonhulpujg7cl160bovtrhn5a7r_lyzob7qzz_c42imf57spf5t0lq9dhv17dzwx722l"))}
testObject_OtherMemberUpdate_user_2 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_2 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "pkrak8z16p5yfu4yvbto1e5s80tzse9nj5pwzbfp95jnch2ie5icdwopw2h3a120r2h27jdl5yo8xrj19lu59v02dn3f5lko_5zv43_0bk0zl"))}
testObject_OtherMemberUpdate_user_3 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_3 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "a4isnmmvwiqz0z43753jryac_69wikztqpduzmxzkjyx39elo8msw_w7k0sxcde68rojxjlgxqx988118vb3cuqtekhi"))}
testObject_OtherMemberUpdate_user_4 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_4 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "efoodlbhjbg1wsh1g4g79zmali8hwl_y4a60dkxpnlv8nceejako8ueebr92c523esviu341rvzy_cump3z41lzsfz"))}
testObject_OtherMemberUpdate_user_5 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_5 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "7e4295v6kr51nc4mjo85jckoqj1x0m0quw1njk1hba5usak_vrcu92k3hn_ekjij_vn8_q6gysplvvg23y_h65p5"))}
testObject_OtherMemberUpdate_user_6 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_6 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "ab3haoinb6anvv5__p1ka8vbcoxxr4ivh5u56kktc3noii5e4vdzcjqj4n6agkg_o74oe1v0lbm3iby4d9w6dd5v7jpu4ttzv86"))}
testObject_OtherMemberUpdate_user_7 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_7 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "4q_ii2nrg4hurzbpr25gik245_hdebddz7nmnkoed4ige383pe4_xiooi9ptxj5_nnt0e7kllbrrn7kezidjhgxetixrnzh93hofkkpu7b9xmavlkc3hda9nppu67yx"))}
testObject_OtherMemberUpdate_user_8 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_8 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "lr2yc8aqus657q8wus3bq1tcjr2wbujoy8derdf8zgsle364fehkvtmcxryhouvikq1ttqo1281_c_lfos28sq5zci7my6ff0tkocxd6iphl39su3b8vr40cy9a"))}
testObject_OtherMemberUpdate_user_9 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_9 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "s_8avhr9nc5pud2dra6o2z5lbvs68"))}
testObject_OtherMemberUpdate_user_10 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_10 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "mxatencn3a0b6g35llbn74_zedl4y_e071egzcfp67mh4ldwum28valjh6yh_mglj"))}
testObject_OtherMemberUpdate_user_11 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_11 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "81lwodm4xlozxhxe72tlz739_08x9k3rv"))}
testObject_OtherMemberUpdate_user_12 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_12 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "8nmmqx87pi1ocp1nmbr9mv2c9upsg04dcto9q4j_whi1aoo6eb0mtjzq3oo07z"))}
testObject_OtherMemberUpdate_user_13 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_13 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "vus3v_jpl0a_mduz6o6dprnmjfaifzootn5q9kf5nyf93a64be8tc7xo3ij_3jpkk1d5xcwqt5hgobeh3w2__qre7vc_mbr"))}
testObject_OtherMemberUpdate_user_14 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_14 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "r5aauwrt372t5ksb5ich_on2z52t29d5y4v2czy0ltlc6m03rttmmkcwvg_chqiv4m8d27jc52rp20y1acczhlzn"))}
testObject_OtherMemberUpdate_user_15 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_15 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "l1_vxlkure3m6u06ydz6gfbz6cx8dk4du9in45c6u9p_yph2j5aews5id_yagwy5vgo94i0lboifdqdk6w5"))}
testObject_OtherMemberUpdate_user_16 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_16 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "0rnlbea812i9aounbq5uzjdpukm3u51l6t_589dlx_m7hjhiwtryjnpuonijfb_18sv6nqc"))}
testObject_OtherMemberUpdate_user_17 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_17 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "c1p1ctbm5b3aqiknqgccubkicla6mw_28p8nr4xu6ujahnwwcuf19s4f6gx1mkt3ehpgeov5b7rpdhz3jgdty13fhf4"))}
testObject_OtherMemberUpdate_user_18 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_18 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "p0mzrv_1f4lu8w7thgqp7pm1fcyowjd68wk0os29rj5xdva84yztt3e9fy74c8lnwguf4sbtqk305v7z4maad4wbc7oqz71d2yf6d8chme55zfd51_scbhhny"))}
testObject_OtherMemberUpdate_user_19 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_19 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "lc4aujgtcjlkscsl2ku3k22c_c68d7cqo092l72_ia7rgaen870p3scm3uzn2v5jol7y026o0we2mvyoc5vm4m8248n4rv40upvd05mq87tts7ffxi3em"))}
testObject_OtherMemberUpdate_user_20 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_20 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "h9_65ijdv0x03vvkrq3e_shbg75ag4tbgr"))}
