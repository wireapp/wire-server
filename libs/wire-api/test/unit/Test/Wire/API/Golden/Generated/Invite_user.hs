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
testObject_Invite_user_1 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000007d-0000-000f-0000-004b0000002a")))])), invRoleName = (fromJust (parseRoleName "pcnk71mkgy4x5m5pyu_buramul48ife7l_sdshnl_fgnvssc4_2reyp0_2dzpiprot"))}
testObject_Invite_user_2 :: Invite
testObject_Invite_user_2 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000050-0000-0018-0000-00390000002d"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))])), invRoleName = (fromJust (parseRoleName "f8c3i0ev7rd7lyqpw6jrvj00ioa_tpv9ymrukq8o699iuz6uzrzfuwra_cldwqd7snwrwn"))}
testObject_Invite_user_3 :: Invite
testObject_Invite_user_3 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000005c-0000-0021-0000-00120000000e"))),(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001")))])), invRoleName = (fromJust (parseRoleName "mjkpjs"))}
testObject_Invite_user_4 :: Invite
testObject_Invite_user_4 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000035-0000-007c-0000-006300000041")))])), invRoleName = (fromJust (parseRoleName "j_736_g07c"))}
testObject_Invite_user_5 :: Invite
testObject_Invite_user_5 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000036-0000-005d-0000-00010000001d"))),(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))),(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002")))])), invRoleName = (fromJust (parseRoleName "_94emt2778geec223527m_1kqhg68oeyqkb20s_ox7wfazugvxaqw6i0lhw150nfbljme0brvduzyiph8_bdq0js0ww7u0ywg0misaygkr6ud2pc0l2yjhvpq7af"))}
testObject_Invite_user_6 :: Invite
testObject_Invite_user_6 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000062-0000-004f-0000-005d0000006e"))),(Id (fromJust (UUID.fromString "0000002e-0000-0078-0000-00430000001e")))])), invRoleName = (fromJust (parseRoleName "d21gudqi_ye5i2xvnt5si3lybkt"))}
testObject_Invite_user_7 :: Invite
testObject_Invite_user_7 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000004b-0000-0055-0000-005f0000001e"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002")))])), invRoleName = (fromJust (parseRoleName "tsrpw7oxcharbhwygjiza7563jyn8bisugadi8"))}
testObject_Invite_user_8 :: Invite
testObject_Invite_user_8 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000002a-0000-0060-0000-003300000058")))])), invRoleName = (fromJust (parseRoleName "b8r_gbzphrjg9e4fv0vcxkr5e9g9oi852p7cwi6mik_cq8uirj3kutb9b3t0y7owz7lr7j2oxwldvlax9u61adg__t8rflu9kwjd"))}
testObject_Invite_user_9 :: Invite
testObject_Invite_user_9 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000039-0000-004d-0000-001a00000009"))),(Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000500000001"))),(Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000500000008")))])), invRoleName = (fromJust (parseRoleName "tra6w3pd37pcciaqvs5061f_36jsyfzh0"))}
testObject_Invite_user_10 :: Invite
testObject_Invite_user_10 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000048-0000-0007-0000-004800000063"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002")))])), invRoleName = (fromJust (parseRoleName "7z7z2jhqwekigj7bw23howg5sjbdqkdv2zi7hxf3sb8j45y0nc0g7xhs_p9"))}
testObject_Invite_user_11 :: Invite
testObject_Invite_user_11 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000001d-0000-0038-0000-007800000051"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001")))])), invRoleName = (fromJust (parseRoleName "wtpd4b8avpu150pqmwdg7qqpaxxly1yx6y3obxjl8qgbk0bo863n8xckio5owvkzsphqtlgrcd_dnr4z_c4tzc4kt1q1e2v3p"))}
testObject_Invite_user_12 :: Invite
testObject_Invite_user_12 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000002d-0000-0039-0000-003200000064")))])), invRoleName = (fromJust (parseRoleName "mdev9wthv7oylbr15ug3t4ws643ekr3my8ul9c1hg8ivxdkvgbmgl_vx3__pi3xlxhky_f74irtdlaq7o7fzno_b7yxje"))}
testObject_Invite_user_13 :: Invite
testObject_Invite_user_13 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000009-0000-0027-0000-006b0000001c"))),(Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000300000004"))),(Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000004")))])), invRoleName = (fromJust (parseRoleName "0ayo3lorcccuf_dksdljqyzmq396yne52umqh5qtqbrgu46vuo9uz4f942ggmrsy2h"))}
testObject_Invite_user_14 :: Invite
testObject_Invite_user_14 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000039-0000-007a-0000-00410000005a"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000400000005")))])), invRoleName = (fromJust (parseRoleName "05lpcqngbmke4mysrlhcwif5g_y_lwfl3aligsy1xn6m_ab008motiql2gqp5wiup7b9blqkal2wc61rrw2hmbx3smvdc0zo14dg"))}
testObject_Invite_user_15 :: Invite
testObject_Invite_user_15 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000006a-0000-004e-0000-007a00000037"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))])), invRoleName = (fromJust (parseRoleName "ncuudrivvjpcwe8g75yhg02mt74p88rwj0l63j1ww7i_tmf29gur_dtwas7zdumhz0o6so97r2hb5xvqo46u54q4n3l4ewwfxods"))}
testObject_Invite_user_16 :: Invite
testObject_Invite_user_16 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000054-0000-0009-0000-004600000007")))])), invRoleName = (fromJust (parseRoleName "dxg8bv9y18hgpr782ddxmxzlxod_z8jfkv00i6tv1_c_p306hratv9z063wkr7at0vl"))}
testObject_Invite_user_17 :: Invite
testObject_Invite_user_17 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000000c-0000-0013-0000-00680000000f"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002")))])), invRoleName = (fromJust (parseRoleName "ua_c2qs9_v37plp05p7tflzwvwasz8o8e_9c6q0sut5zq"))}
testObject_Invite_user_18 :: Invite
testObject_Invite_user_18 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000062-0000-0003-0000-00000000006b")))])), invRoleName = (fromJust (parseRoleName "8_ojk2f0q3wj90c"))}
testObject_Invite_user_19 :: Invite
testObject_Invite_user_19 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000058-0000-0001-0000-003700000063"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))),(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))),(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002")))])), invRoleName = (fromJust (parseRoleName "83ofc8n0jd18lbjm0hljro1iejn4mgq1i_kd0g6bj0d0m1d"))}
testObject_Invite_user_20 :: Invite
testObject_Invite_user_20 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000059-0000-003f-0000-004d00000067"))),(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000008"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))])), invRoleName = (fromJust (parseRoleName "gqao78lpqmzlcvgw66_sz31cdl8d5_6yjoto5cs8pqegbeeki_nph4adlv5egwtx0n8hcksfc55_tysd54dweb7bxe2myso1nl6mz2hzwkwi2yzh_k_"))}
