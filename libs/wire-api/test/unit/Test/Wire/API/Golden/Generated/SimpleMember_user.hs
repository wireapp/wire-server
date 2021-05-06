{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.SimpleMember_user where

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
testObject_SimpleMember_user_1 :: SimpleMember
testObject_SimpleMember_user_1 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000036-0000-004d-0000-002000000072"))), smConvRoleName = (fromJust (parseRoleName "893otnqgwpm4cvrgunoota3"))}
testObject_SimpleMember_user_2 :: SimpleMember
testObject_SimpleMember_user_2 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001d-0000-0033-0000-004500000004"))), smConvRoleName = (fromJust (parseRoleName "suw06irml_ke3yhe0v5niyt9089jm8ckeyrpz4fpaq"))}
testObject_SimpleMember_user_3 :: SimpleMember
testObject_SimpleMember_user_3 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001c-0000-006b-0000-00600000005a"))), smConvRoleName = (fromJust (parseRoleName "z7aj36vs1ozoz6ez73h7o_fnzbqolywg380t04o9ekuug57kc9ebea1ac9_98zjhlrskgxrbw0__vaqp4mecvi726py4npz45_cxpaqc_vgz8zgxm29m929bsi"))}
testObject_SimpleMember_user_4 :: SimpleMember
testObject_SimpleMember_user_4 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000005d-0000-0052-0000-002e0000001a"))), smConvRoleName = (fromJust (parseRoleName "_c1gs_s"))}
testObject_SimpleMember_user_5 :: SimpleMember
testObject_SimpleMember_user_5 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000014-0000-002f-0000-006a0000006c"))), smConvRoleName = (fromJust (parseRoleName "8bkug3glyrq4_sk93l4ikj6lsaazm9suqemqywtjp6fll4bnb_4sn"))}
testObject_SimpleMember_user_6 :: SimpleMember
testObject_SimpleMember_user_6 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000039-0000-0002-0000-007e00000055"))), smConvRoleName = (fromJust (parseRoleName "6zelu87p8uty62cyxxrs46xqtjdec9k6feccx0k6p4xm875d26f5m8szmr8"))}
testObject_SimpleMember_user_7 :: SimpleMember
testObject_SimpleMember_user_7 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000075-0000-0002-0000-007400000063"))), smConvRoleName = (fromJust (parseRoleName "4cxasmkvpywo5o3zbozbb_elsxhl6_ny0bdv4prueoeeakp3g23uaui8j5zb_nwvcr0gul770v5z00xglp2_764v6p71ga3xmai66soo2o1sermrgkqy"))}
testObject_SimpleMember_user_8 :: SimpleMember
testObject_SimpleMember_user_8 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000040-0000-0020-0000-001b00000027"))), smConvRoleName = (fromJust (parseRoleName "gex9x_ullez2e65b9lfcc7d9lrfn05gahgdwq2x9r491d4tgkt4pa46bcnvw_hk9eeqdjwamprx7m9vuqmu8z0ch7m0gd0a7x064g6yl"))}
testObject_SimpleMember_user_9 :: SimpleMember
testObject_SimpleMember_user_9 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000023-0000-0000-0000-000a00000034"))), smConvRoleName = (fromJust (parseRoleName "ltm8p1vo5003lsnowm3q1kxnp5d"))}
testObject_SimpleMember_user_10 :: SimpleMember
testObject_SimpleMember_user_10 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001d-0000-000d-0000-00720000003a"))), smConvRoleName = (fromJust (parseRoleName "qv1m0_100573x_emal9g8qqs_igs5iu5kkhy8tx2eeer1a77daxb0jd9elb40u_akxmc9p9zsqiqsg6yxu1503j1qvzc5pibg2no87p436ei4t7ys"))}
testObject_SimpleMember_user_11 :: SimpleMember
testObject_SimpleMember_user_11 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000004-0000-0051-0000-005d0000000e"))), smConvRoleName = (fromJust (parseRoleName "o9w8esmt_dm_o18g7985mqndo7rnsgay_tmaz5eh82r3867u9ekwk7eqnzw4v03gu6z6"))}
testObject_SimpleMember_user_12 :: SimpleMember
testObject_SimpleMember_user_12 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000033-0000-004f-0000-00050000003b"))), smConvRoleName = (fromJust (parseRoleName "ne1762p_koyzi05z2gs1pyc5d0ouhqtovlf5t_xk4rne6585igh8w0m8_yi6y4y5cimpzdr3yqxdpyajemf24m3oygl9qxcjbh12_j_by2q7wtqy44z1gd_wh"))}
testObject_SimpleMember_user_13 :: SimpleMember
testObject_SimpleMember_user_13 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000048-0000-005a-0000-001d00000028"))), smConvRoleName = (fromJust (parseRoleName "tu7fbwqb7tjhe69f9aisjwjs6l66pedh"))}
testObject_SimpleMember_user_14 :: SimpleMember
testObject_SimpleMember_user_14 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000002a-0000-0019-0000-00690000005d"))), smConvRoleName = (fromJust (parseRoleName "emr3yi9t4mbedsy8sl212knwg5rwd7fy7xpvnxwp2k113vi6lb39t1jy0yknuznxoqke540rmrwshb_tlw"))}
testObject_SimpleMember_user_15 :: SimpleMember
testObject_SimpleMember_user_15 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000045-0000-0010-0000-00030000006c"))), smConvRoleName = (fromJust (parseRoleName "4ho3m7rne7umoxg4jzpk0rfeu29r0cpdpim0cts4_tto9e0cltv9mkorxhx4nqt1xf6s_g30ur1ku2lishk00cq8xgi9y06jt2jp5d3"))}
testObject_SimpleMember_user_16 :: SimpleMember
testObject_SimpleMember_user_16 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000043-0000-0067-0000-002b00000063"))), smConvRoleName = (fromJust (parseRoleName "zz3naekc"))}
testObject_SimpleMember_user_17 :: SimpleMember
testObject_SimpleMember_user_17 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000006-0000-0060-0000-003a0000003e"))), smConvRoleName = (fromJust (parseRoleName "a8cctkaad685_r7v1arbh2rawhu9npeczkc0_qohd0f1ap27thsy7rgpn91os8jloajiqtqfqsx"))}
testObject_SimpleMember_user_18 :: SimpleMember
testObject_SimpleMember_user_18 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006f-0000-0025-0000-003500000009"))), smConvRoleName = (fromJust (parseRoleName "clq_r23fdwyqgv_zwips608tz1cx4t94o2nf07wgfw2qfqyz4j4g484ne35p4ezmz4j5unxmbrpei629ms4byohulow9jw25tcjds_o1h3hbq1c1enui3ehksdoh5"))}
testObject_SimpleMember_user_19 :: SimpleMember
testObject_SimpleMember_user_19 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000074-0000-0000-0000-007500000052"))), smConvRoleName = (fromJust (parseRoleName "xbio73fv7a57y5nzs03vqaa3j9emoz8pd_idly28a"))}
testObject_SimpleMember_user_20 :: SimpleMember
testObject_SimpleMember_user_20 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006c-0000-0065-0000-001b0000004b"))), smConvRoleName = (fromJust (parseRoleName "l4rfrb"))}
