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
testObject_Invite_user_1 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000005e-0000-000e-0000-006a00000020")))])), invRoleName = (fromJust (parseRoleName "qhf56yjuovm5obtmarl_msjpph6l4ksv7jn3g3sgdbaobwttr7t5nbb1ltrav3dc3r3vphszmbu327k8v7ykgdso3aikt7k1jkacnuug6umvlblyd4ukz6d7gckznc3"))}
testObject_Invite_user_2 :: Invite
testObject_Invite_user_2 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000002-0000-0057-0000-006d00000002")))])), invRoleName = (fromJust (parseRoleName "y7ocfmukmqndpifollm6ze8o9c920p1r1ha0zb6veokk7y"))}
testObject_Invite_user_3 :: Invite
testObject_Invite_user_3 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000005a-0000-004a-0000-007400000061"))),(Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000800000007"))),(Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000300000006")))])), invRoleName = (fromJust (parseRoleName "3s0kjxzo_199pwm0mc2ren266owcdm48pzrhfv7fzgdhfcwi4rbpt425ol754gcekau2b"))}
testObject_Invite_user_4 :: Invite
testObject_Invite_user_4 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000028-0000-0017-0000-000200000079"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))])), invRoleName = (fromJust (parseRoleName "rlzf3hi2cxe4l8otml64g7v"))}
testObject_Invite_user_5 :: Invite
testObject_Invite_user_5 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000036-0000-0024-0000-005300000009"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))])), invRoleName = (fromJust (parseRoleName "6noz64u9jd79yp6om87ouzzwv84j7vsl59om6rs4rl8j9fsy55udu6chac2joy_9fo_pub5axptqw"))}
testObject_Invite_user_6 :: Invite
testObject_Invite_user_6 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000045-0000-0048-0000-005c0000000f"))),(Id (fromJust (UUID.fromString "0000003a-0000-000f-0000-006e00000070")))])), invRoleName = (fromJust (parseRoleName "1liet"))}
testObject_Invite_user_7 :: Invite
testObject_Invite_user_7 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000005-0000-004f-0000-004900000000")))])), invRoleName = (fromJust (parseRoleName "p086ktl05ox_ier8oi6c5vrhpac1bbzl736_6ygatusbqyv9m_fpl3twy17o7na"))}
testObject_Invite_user_8 :: Invite
testObject_Invite_user_8 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000002d-0000-002a-0000-00710000000c"))),(Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000500000005"))),(Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000100000004")))])), invRoleName = (fromJust (parseRoleName "awl2jpca_dl7o275j0k9l69o_agwhslgqnz6_0pfqilngrkoi72sug6q7jfguaw3jh9ij0_oo9gq834ydq7oiga82bfcy4h2_3lxlffy99arn4q1hnr46tk"))}
testObject_Invite_user_9 :: Invite
testObject_Invite_user_9 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000007f-0000-003a-0000-000c0000007f"))),(Id (fromJust (UUID.fromString "00000037-0000-0049-0000-007300000007")))])), invRoleName = (fromJust (parseRoleName "j2umuheb5zqx56gmos86vzr5ngt3vwv67qnu64k35m9"))}
testObject_Invite_user_10 :: Invite
testObject_Invite_user_10 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000031-0000-007a-0000-001500000046"))),(Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000400000002"))),(Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000300000000")))])), invRoleName = (fromJust (parseRoleName "w_c105orc8kd6s4"))}
testObject_Invite_user_11 :: Invite
testObject_Invite_user_11 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000033-0000-004d-0000-00610000006e"))),(Id (fromJust (UUID.fromString "0000002b-0000-000b-0000-006900000026")))])), invRoleName = (fromJust (parseRoleName "30twv59ffs7z2mqcqy2umesa1umtmz3zfcf"))}
testObject_Invite_user_12 :: Invite
testObject_Invite_user_12 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000040-0000-0018-0000-003700000047"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))])), invRoleName = (fromJust (parseRoleName "tcc0ze56rsbgwno939tknj2yi2oye0ps0yg0o51kn9wv51okn205lj5ig24gfv"))}
testObject_Invite_user_13 :: Invite
testObject_Invite_user_13 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000002a-0000-0074-0000-00210000003d"))),(Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000000000008"))),(Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000100000005")))])), invRoleName = (fromJust (parseRoleName "n7w_agn"))}
testObject_Invite_user_14 :: Invite
testObject_Invite_user_14 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000030-0000-007a-0000-000200000032"))),(Id (fromJust (UUID.fromString "00000062-0000-0006-0000-004b00000056")))])), invRoleName = (fromJust (parseRoleName "wgyu5n4tmlupqkkcgucotcxwlrpd2okel_dqquy4kti81675x3qlxr62kppa8ieo93b8vvxr56"))}
testObject_Invite_user_15 :: Invite
testObject_Invite_user_15 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000002d-0000-0024-0000-00410000007b"))),(Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000000000002"))),(Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000000000000")))])), invRoleName = (fromJust (parseRoleName "akigsr60_js7tu_5w51tvz1zuaxmro_knkrxywmu7fv1u_g676vkbkfikgkun80obbm3ivpz6v0c5xvdlq08df59w_onyw4ejq12oir8ux2e4wfrdeawjregsq5cbat"))}
testObject_Invite_user_16 :: Invite
testObject_Invite_user_16 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000002c-0000-0051-0000-006600000045"))),(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))),(Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000300000002"))),(Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000200000004")))])), invRoleName = (fromJust (parseRoleName "6fgsslxa9yfme17b_hpdy1lc7qu_r0yyt5ok357w8tch25xbajjff5sgst9dquxqizoimmnzvlhih2jktb"))}
testObject_Invite_user_17 :: Invite
testObject_Invite_user_17 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000048-0000-0038-0000-005700000013"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))])), invRoleName = (fromJust (parseRoleName "9uemlik2btyjupaif7ch3zaeb9pmazec86z87snzytrbxdzawiszsah05mahb9ly1u28khzx7ee9l9suh4yy7yq"))}
testObject_Invite_user_18 :: Invite
testObject_Invite_user_18 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000025-0000-0050-0000-00020000000c"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))])), invRoleName = (fromJust (parseRoleName "y6x23lua_eisl3k9ko0mfqli_u96wsw0lc0xxqf3ypypl6fl13vyksr7o1a5p0z8mr0spm3"))}
testObject_Invite_user_19 :: Invite
testObject_Invite_user_19 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000050-0000-0026-0000-00310000002e"))),(Id (fromJust (UUID.fromString "00000013-0000-0000-0000-00600000006e")))])), invRoleName = (fromJust (parseRoleName "4vcd22uxnkqhe4l8r3celsr8yp3zg489ik_aaf4h523039c_mhvzq91_9c2xtbh90cc72nez1s"))}
testObject_Invite_user_20 :: Invite
testObject_Invite_user_20 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000025-0000-004d-0000-00720000004b"))),(Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000005-0000-0001-0000-000800000003")))])), invRoleName = (fromJust (parseRoleName "pw9i_v7_fp3iw26u3s98om20"))}
