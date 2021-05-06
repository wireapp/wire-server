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
testObject_SimpleMember_user_1 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000008-0000-0071-0000-00490000001d"))), smConvRoleName = (fromJust (parseRoleName "w64mwzubgz590g0vxqmgfgpc1essytpbko4ziwss7po5a1yohubjeak4xb2sgksb60"))}
testObject_SimpleMember_user_2 :: SimpleMember
testObject_SimpleMember_user_2 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000b-0000-0027-0000-00680000002e"))), smConvRoleName = (fromJust (parseRoleName "976pjwzbh4lsg0p0karf1n8jg_hubp9wyhckfz8ki9vzccc2cnmzgv5tpz90g1em006d9l_vj5kg32"))}
testObject_SimpleMember_user_3 :: SimpleMember
testObject_SimpleMember_user_3 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000d-0000-0008-0000-000f00000068"))), smConvRoleName = (fromJust (parseRoleName "24wwklo1t_i1l6xvg7dl3iy2xcfn8c3c4so4gskr8vrb9b3kki34u1sjhfm07j_0oq5iuikf51ni8tyig48"))}
testObject_SimpleMember_user_4 :: SimpleMember
testObject_SimpleMember_user_4 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000020-0000-0075-0000-004300000037"))), smConvRoleName = (fromJust (parseRoleName "_w4klatkq5vc5xr8gywg1dyd1g3faa96j3zly69g74pxbzxilds_xets85kd5k8b1x3aa1hz5jj_86fvhpa99w5syh"))}
testObject_SimpleMember_user_5 :: SimpleMember
testObject_SimpleMember_user_5 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000005b-0000-003b-0000-007c00000013"))), smConvRoleName = (fromJust (parseRoleName "77n2xta3fzc"))}
testObject_SimpleMember_user_6 :: SimpleMember
testObject_SimpleMember_user_6 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000078-0000-0070-0000-00700000006c"))), smConvRoleName = (fromJust (parseRoleName "n_ionydxsmjes"))}
testObject_SimpleMember_user_7 :: SimpleMember
testObject_SimpleMember_user_7 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000004c-0000-0017-0000-002e00000078"))), smConvRoleName = (fromJust (parseRoleName "mbb"))}
testObject_SimpleMember_user_8 :: SimpleMember
testObject_SimpleMember_user_8 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000080-0000-0002-0000-00160000001e"))), smConvRoleName = (fromJust (parseRoleName "qrs4kr7wup1gnfk16rkp1t5gp846jw0qspw77v7u05e_f95amp8cayc2v35bx5_83p"))}
testObject_SimpleMember_user_9 :: SimpleMember
testObject_SimpleMember_user_9 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000054-0000-0022-0000-002200000029"))), smConvRoleName = (fromJust (parseRoleName "_gib0e3lcukinpypaxujru86_c7pf362a9pxcncy4re2itxw1mu9fvqd3qybhdedelmdmd3o2g5afr9dqe5duebe_llpryotid516mffht52esazuvnpehk48a7dh0"))}
testObject_SimpleMember_user_10 :: SimpleMember
testObject_SimpleMember_user_10 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003e-0000-005f-0000-006600000030"))), smConvRoleName = (fromJust (parseRoleName "j66ccrokfc77ohsy_7nonh"))}
testObject_SimpleMember_user_11 :: SimpleMember
testObject_SimpleMember_user_11 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000080-0000-002f-0000-007f0000004b"))), smConvRoleName = (fromJust (parseRoleName "jzt"))}
testObject_SimpleMember_user_12 :: SimpleMember
testObject_SimpleMember_user_12 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000078-0000-007f-0000-003300000068"))), smConvRoleName = (fromJust (parseRoleName "_596m92gprg0rxqq_mybdy55wixkr9ba0ar9ulkh8nd3pnuxqt6iapn13ya3l_e9mwz4jpfbhc"))}
testObject_SimpleMember_user_13 :: SimpleMember
testObject_SimpleMember_user_13 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000003c-0000-0018-0000-004c00000056"))), smConvRoleName = (fromJust (parseRoleName "dmxo4w8j3va_qgwmi85v4201_2c4kh5iqhm0zp5tn8zlh2prrkb311wg76l9aqn_kfztfn781wpj3guvadbykhy6qp3n_ai63fetoxw8lnigrthih32ecx7"))}
testObject_SimpleMember_user_14 :: SimpleMember
testObject_SimpleMember_user_14 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000074-0000-0061-0000-007b00000002"))), smConvRoleName = (fromJust (parseRoleName "0egs6ey0o4k9hmfkwfziakoszxpmb1om0du2v35150okkwtnqo1u9q"))}
testObject_SimpleMember_user_15 :: SimpleMember
testObject_SimpleMember_user_15 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000071-0000-006e-0000-00210000003a"))), smConvRoleName = (fromJust (parseRoleName "4ynk40b_5h2nxfjaa9lpiofzqb3qynawnte29hew6wpmd3h7wdc8aaxnl0goujdqy5i9t6kilr7fuyj4wuxix"))}
testObject_SimpleMember_user_16 :: SimpleMember
testObject_SimpleMember_user_16 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000048-0000-000b-0000-007e0000002c"))), smConvRoleName = (fromJust (parseRoleName "lr03x4lwt3uawyh57av1fbkr5no_fvw1ta6clkykzfhes3o_slt8tricof0sejgmcj99arija2hy4j8mrw5th7vkq"))}
testObject_SimpleMember_user_17 :: SimpleMember
testObject_SimpleMember_user_17 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000038-0000-0057-0000-001200000066"))), smConvRoleName = (fromJust (parseRoleName "rs6a0gvif8p39flwfmmn79ps8l6yp7atl5hg3p_94wviz85buiappmzxa_264rc46b6rgux28imbrvt1yt0h"))}
testObject_SimpleMember_user_18 :: SimpleMember
testObject_SimpleMember_user_18 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000009-0000-004d-0000-002900000042"))), smConvRoleName = (fromJust (parseRoleName "c2pzxr88cug68439ngsereenpxtuyy4no_c7jrym94sqd41au_ba45op7ucr56pkg464lu3wpdkma3mkmju712nfw6jpais23cn97"))}
testObject_SimpleMember_user_19 :: SimpleMember
testObject_SimpleMember_user_19 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007b-0000-0013-0000-00250000001a"))), smConvRoleName = (fromJust (parseRoleName "c7ew8qg2w_d6jd6a1t02zyuyumzduatzzj9z606x1ajw2vcaqucdtv4ao7x_0e5s2ygws3d6elyj4"))}
testObject_SimpleMember_user_20 :: SimpleMember
testObject_SimpleMember_user_20 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000e-0000-0042-0000-004b00000010"))), smConvRoleName = (fromJust (parseRoleName "2fxef55"))}
