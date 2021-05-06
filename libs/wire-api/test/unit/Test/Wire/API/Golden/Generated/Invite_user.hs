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
testObject_Invite_user_1 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000033-0000-006c-0000-000900000063")))])), invRoleName = (fromJust (parseRoleName "xsuqeopoapl00ceixx_oimivkcwx5f00xvu6nsp5ntvofj6typ_nl8m9"))}
testObject_Invite_user_2 :: Invite
testObject_Invite_user_2 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000076-0000-0015-0000-00540000006c"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000")))])), invRoleName = (fromJust (parseRoleName "we8mw8q9f43607d3x9rh68yu_69zittx2goax5xzxxekfnwbz1t2zsjgkh15fag2xkr6pjxc4zf2ncxjf0v2id_usnwkq0aamql0nyf3nmn7tz3koaz1m0kyqwg"))}
testObject_Invite_user_3 :: Invite
testObject_Invite_user_3 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000001-0000-0010-0000-003e0000005c"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))])), invRoleName = (fromJust (parseRoleName "365jeo011b2le566mtdriedjt_e8rgkxp_xkfzilizlybseg2p1g49g67u_axevmx9yqenrgmn"))}
testObject_Invite_user_4 :: Invite
testObject_Invite_user_4 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000013-0000-001d-0000-005b00000073"))),(Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000800000004"))),(Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000006")))])), invRoleName = (fromJust (parseRoleName "6wh_mblk8vrbyyxer9qhhgi1ggt2i7js1q7rh_qu__lujf3z0wef7tsfok379f1brkbmhr2uuiedpeqw1p"))}
testObject_Invite_user_5 :: Invite
testObject_Invite_user_5 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000003c-0000-0037-0000-004100000049")))])), invRoleName = (fromJust (parseRoleName "2n0z9f3r2_oaemukxxsp"))}
testObject_Invite_user_6 :: Invite
testObject_Invite_user_6 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000058-0000-0052-0000-00050000004f"))),(Id (fromJust (UUID.fromString "00000017-0000-0008-0000-003300000059")))])), invRoleName = (fromJust (parseRoleName "u_ot8t26rhlof6hbbv_m71_tcu2igqme4g4inbhgqp4h25cz8s4bi2llzijyjaxtw881tt7oy7spvj9_k3pl_3o"))}
testObject_Invite_user_7 :: Invite
testObject_Invite_user_7 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000060-0000-000f-0000-002500000065"))),(Id (fromJust (UUID.fromString "0000003b-0000-0072-0000-002f00000006")))])), invRoleName = (fromJust (parseRoleName "6tqeg8po7t25_o1lsu3veccqo1ube2hotx335asjt5me68b0ihtsdld9jjv6_n26pdctck8alswmyi"))}
testObject_Invite_user_8 :: Invite
testObject_Invite_user_8 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000072-0000-000f-0000-002b0000004d"))),(Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000800000008"))),(Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000600000000")))])), invRoleName = (fromJust (parseRoleName "9a1i3w4og14txk0ni3v9ubnk8jj72wd_fu4wyczbq3252vvj4ccv9h"))}
testObject_Invite_user_9 :: Invite
testObject_Invite_user_9 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000001f-0000-007a-0000-001c00000012"))),(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))),(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))),(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001")))])), invRoleName = (fromJust (parseRoleName "t5xukt5t2gnpq_4qqd0iz6_o4mhnz7mif3v3gvvv3vhzlb2hkh400"))}
testObject_Invite_user_10 :: Invite
testObject_Invite_user_10 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000004d-0000-001e-0000-002d00000033"))),(Id (fromJust (UUID.fromString "0000000a-0000-0050-0000-00120000007e")))])), invRoleName = (fromJust (parseRoleName "ilr"))}
testObject_Invite_user_11 :: Invite
testObject_Invite_user_11 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000007c-0000-0005-0000-003500000007"))),(Id (fromJust (UUID.fromString "0000006f-0000-003c-0000-007300000067")))])), invRoleName = (fromJust (parseRoleName "n1dufaw76p8bwbx8gyxn2o04zfmgym7frzt9_gtkce_ehk2j2gsab1dhoxljqt7oqu36tvnt2xe_l4icjgi0hoh20h6dsqdqzx7nrjjrhvxunzkjvjx9b"))}
testObject_Invite_user_12 :: Invite
testObject_Invite_user_12 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000003d-0000-002b-0000-006600000030"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001")))])), invRoleName = (fromJust (parseRoleName "aiy8x4obatf017v0d64bx8098gafguy33znlpv1a0453k8p13x_3oyxmz1934ujc4u__apd4kaazot8lr3ul4_9rodbtfcr0n"))}
testObject_Invite_user_13 :: Invite
testObject_Invite_user_13 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000017-0000-002c-0000-007000000065"))),(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000300000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002")))])), invRoleName = (fromJust (parseRoleName "jr8i704qjhmxtvscrfcgr3ko5echg09xbtpup3w8reryb2gibekackr6a7bh6lwtnhsht6iz2t"))}
testObject_Invite_user_14 :: Invite
testObject_Invite_user_14 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000005f-0000-005c-0000-000b0000007b"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))])), invRoleName = (fromJust (parseRoleName "cpbwg27pzg14ohzzj18yu8j19_1_xyp2v_9tdk9ddoxz0aoy0k1kg7tw"))}
testObject_Invite_user_15 :: Invite
testObject_Invite_user_15 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000076-0000-000a-0000-000b00000046")))])), invRoleName = (fromJust (parseRoleName "8qzx1rdfleu6c9y6zzq_ik79ucmfuwj10u15m2jgmqrlvhrmw8f87o5seualu131c8yg__8jz1zcp7zul4gaognoq7fjzu55t5lyf5wrox45qe9"))}
testObject_Invite_user_16 :: Invite
testObject_Invite_user_16 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000003-0000-0061-0000-001f00000058"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))])), invRoleName = (fromJust (parseRoleName "lh2axwcsjuzg5kb_erj_f9cgkzbogy3336_g3rzyqou"))}
testObject_Invite_user_17 :: Invite
testObject_Invite_user_17 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000068-0000-0013-0000-008000000029"))),(Id (fromJust (UUID.fromString "00000060-0000-006f-0000-004700000063")))])), invRoleName = (fromJust (parseRoleName "p6ggnml"))}
testObject_Invite_user_18 :: Invite
testObject_Invite_user_18 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000040-0000-0003-0000-004500000074"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001")))])), invRoleName = (fromJust (parseRoleName "aqkxg5emrslrp9j092spj44j6t7hj4u3r38hqpzfzd4_nuv2c8pciu4jsmfp"))}
testObject_Invite_user_19 :: Invite
testObject_Invite_user_19 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000005e-0000-003b-0000-005500000067"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002")))])), invRoleName = (fromJust (parseRoleName "jmz3_mvtpgeft8ihuyolaksx_o5jc36y4o_l8tbr9hgyihk7mev5pqeqwoy9prhyls2fhz15yzuotzu_w6f8tb5i_9s2brr19swsksclx_n"))}
testObject_Invite_user_20 :: Invite
testObject_Invite_user_20 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000002f-0000-0014-0000-00100000002d"))),(Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000600000000"))),(Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000000000006")))])), invRoleName = (fromJust (parseRoleName "x77ffot_xzmrxxvkdp5pxlfoeaq4cck5wskfsfbwoco9k34pe25j7171l70tvzn"))}
