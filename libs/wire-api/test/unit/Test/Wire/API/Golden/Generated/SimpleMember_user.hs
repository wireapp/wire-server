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
testObject_SimpleMember_user_1 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000a-0000-0025-0000-000000000068"))), smConvRoleName = (fromJust (parseRoleName "8t5x0pmrhfkrhc3nrtzt2ks9oxp_1vbjd94welbkkc2k3uc52oqjv0dawijlhps8au0g_lrxq_ekm9fb7muozhzqdw3u0d0"))}
testObject_SimpleMember_user_2 :: SimpleMember
testObject_SimpleMember_user_2 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000037-0000-0030-0000-004e00000033"))), smConvRoleName = (fromJust (parseRoleName "ut9ajr739vpvgx676w_"))}
testObject_SimpleMember_user_3 :: SimpleMember
testObject_SimpleMember_user_3 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000077-0000-0042-0000-002900000051"))), smConvRoleName = (fromJust (parseRoleName "amjjig_bwr81wth4zmkmvdxz9ylb6xnnd6xxbuaj600j6ei4ap"))}
testObject_SimpleMember_user_4 :: SimpleMember
testObject_SimpleMember_user_4 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007d-0000-0014-0000-002000000032"))), smConvRoleName = (fromJust (parseRoleName "4u8_xhxolwbg0we_wfrxfpptg3o3hzqdsljsyarqectkrcp4b95bxxe60_xu2mcf"))}
testObject_SimpleMember_user_5 :: SimpleMember
testObject_SimpleMember_user_5 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000041-0000-001b-0000-00780000006b"))), smConvRoleName = (fromJust (parseRoleName "bxzagydmp8h0n6eky9sd0m9mgghlijpnyqtth4axoadi82hz9n3ea94tg9cza2eue6_fdjwlj2jpe19sudmh_93hfm9gfec3v8wz377cdtg4g2"))}
testObject_SimpleMember_user_6 :: SimpleMember
testObject_SimpleMember_user_6 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000052-0000-0061-0000-006400000012"))), smConvRoleName = (fromJust (parseRoleName "1hqlyowbaa0ka_jcxj5d7b_8j1ym2asj47hslvj_qk"))}
testObject_SimpleMember_user_7 :: SimpleMember
testObject_SimpleMember_user_7 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000047-0000-006a-0000-00370000004d"))), smConvRoleName = (fromJust (parseRoleName "6ukwkznuyyuizvzkj9jc"))}
testObject_SimpleMember_user_8 :: SimpleMember
testObject_SimpleMember_user_8 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003e-0000-0054-0000-006d0000006a"))), smConvRoleName = (fromJust (parseRoleName "mpzyilvg1dnwl_4t0i80anf7nl00oh9vxjyrlhy2klm24g76kwqkewpxpbujj5yk5b"))}
testObject_SimpleMember_user_9 :: SimpleMember
testObject_SimpleMember_user_9 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000005f-0000-0051-0000-002d00000044"))), smConvRoleName = (fromJust (parseRoleName "xmrykvrvdi_wvmbm3i93s_x7rpljo_ora0x2xn3crib55z5ysp4dznca6w_7frif3g27wxudn6dr6jfmmwr7ya4qsorzhh3vwfcwyrl8xyqs5te"))}
testObject_SimpleMember_user_10 :: SimpleMember
testObject_SimpleMember_user_10 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000004f-0000-0064-0000-00570000005d"))), smConvRoleName = (fromJust (parseRoleName "7rkdqmgxc8xc33y4e7bkeeudgocdb2l5m1aj_c7nbmi4t4m4_usdj3bs82e2xpul56gvde8fj0zdajj8w"))}
testObject_SimpleMember_user_11 :: SimpleMember
testObject_SimpleMember_user_11 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000034-0000-004e-0000-007c0000001d"))), smConvRoleName = (fromJust (parseRoleName "y_l3ohexbik3xy10ux_qj520cont64ksc__awmouu11ze9cmfie3t9pawpi22q37wlh_avpv7iwemo6w_s45uakxk9hugjt77"))}
testObject_SimpleMember_user_12 :: SimpleMember
testObject_SimpleMember_user_12 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000070-0000-0067-0000-000000000002"))), smConvRoleName = (fromJust (parseRoleName "goc6d"))}
testObject_SimpleMember_user_13 :: SimpleMember
testObject_SimpleMember_user_13 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000077-0000-001c-0000-002300000020"))), smConvRoleName = (fromJust (parseRoleName "fxv9m53065qzrdiz6j_zzzb_c5ntkz0atehh0yf1upla_xcnarxlfrxtxbe01uj5sn6x4mqjadmj2e_1gu1ricfw"))}
testObject_SimpleMember_user_14 :: SimpleMember
testObject_SimpleMember_user_14 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003b-0000-004e-0000-00000000005e"))), smConvRoleName = (fromJust (parseRoleName "4_dzybgupof0xf3k0"))}
testObject_SimpleMember_user_15 :: SimpleMember
testObject_SimpleMember_user_15 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000051-0000-0000-0000-004400000057"))), smConvRoleName = (fromJust (parseRoleName "xs1sacs2n33lh098f0f0ccx7wocu7f6vmm0lwrrx72jllc5aseub0e1i2pn5ufgmxvo3_8h2rt"))}
testObject_SimpleMember_user_16 :: SimpleMember
testObject_SimpleMember_user_16 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000f-0000-0044-0000-001a0000005d"))), smConvRoleName = (fromJust (parseRoleName "4h840alc32q1eeu"))}
testObject_SimpleMember_user_17 :: SimpleMember
testObject_SimpleMember_user_17 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000002c-0000-0075-0000-002a00000046"))), smConvRoleName = (fromJust (parseRoleName "08yatiznk3vcpit0u2x4r27h9okqrxa4oed4ck6g"))}
testObject_SimpleMember_user_18 :: SimpleMember
testObject_SimpleMember_user_18 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006e-0000-007b-0000-003300000071"))), smConvRoleName = (fromJust (parseRoleName "aea9b6mytvdx7rs7nfc2m7ffm25eqsqad2rak_nd"))}
testObject_SimpleMember_user_19 :: SimpleMember
testObject_SimpleMember_user_19 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000030-0000-0057-0000-000900000005"))), smConvRoleName = (fromJust (parseRoleName "w_uoip9tjvko"))}
testObject_SimpleMember_user_20 :: SimpleMember
testObject_SimpleMember_user_20 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000009-0000-007a-0000-00780000000d"))), smConvRoleName = (fromJust (parseRoleName "vzn_1ffq94a5llnjgx4u56f19sg_2imny"))}
