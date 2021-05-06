{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.OtherMember_user where

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
testObject_OtherMember_user_1 :: OtherMember
testObject_OtherMember_user_1 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-001b00000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "jdq3g1rouyh3_43d8uyx3da7gg1wzmfoa328gx914qd58rmhn5rdd0mvwjs12yb33zmlbfszxpv7cbfxedlscrtg2ogi9podkc34cr0xvo9zix82v1ore33yuz"))}
testObject_OtherMember_user_2 :: OtherMember
testObject_OtherMember_user_2 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000005-0000-0015-0000-001900000019"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "s8j2xos42d2ujtdmz4drejn_qmyd44t3y0eq1aj4bpx6smqqdua5r1q5c2j_4f98omqcme4818mcjryxl_vw8r"))}
testObject_OtherMember_user_3 :: OtherMember
testObject_OtherMember_user_3 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000003-0000-000a-0000-000a00000019"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000100000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000001")))}), omConvRoleName = (fromJust (parseRoleName "su94b0m8"))}
testObject_OtherMember_user_4 :: OtherMember
testObject_OtherMember_user_4 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000008-0000-0011-0000-001d0000001e"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000000000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000003")))}), omConvRoleName = (fromJust (parseRoleName "otrxlrsrp5xktoz38y1mj8__ma9hukibem9sti_0gn559ambous9j"))}
testObject_OtherMember_user_5 :: OtherMember
testObject_OtherMember_user_5 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000f0000001e"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "6bmpu05suon2phcqosae9ka81jtxtvjqq1b3nnax2krzx9die4wbuubdm4chbdggvajoi8s86j5_lhirn_zhglrkcujoqx0j9y40jpkxnoq6l4j7tkva8zk2kr7pz"))}
testObject_OtherMember_user_6 :: OtherMember
testObject_OtherMember_user_6 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000004-0000-001f-0000-001600000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000000000003")))}), omConvRoleName = (fromJust (parseRoleName "_paitx4zi9l7y0orlwl93teiqrty9a2e9n8x7ycqzsl2y477yvqawam5rx2hi3xr3qu32362rkyaj5k4cbqhq5vn2cr6"))}
testObject_OtherMember_user_7 :: OtherMember
testObject_OtherMember_user_7 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000003-0000-001f-0000-000e00000012"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "lu3qu8zucb74s1m0hui9boolkvi_hqg9t60oh316rrp_kf6a4eelbz4jn5ia1iho_2m65jhg_9f2qfbz20lnu_9nybsdo"))}
testObject_OtherMember_user_8 :: OtherMember
testObject_OtherMember_user_8 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000017-0000-0020-0000-001300000016"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000004")))}), omConvRoleName = (fromJust (parseRoleName "mbqhcojdv1sw58mw2l2w6ke_p11lu3eev"))}
testObject_OtherMember_user_9 :: OtherMember
testObject_OtherMember_user_9 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000013-0000-001f-0000-00000000001c"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000000000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "7ok9vfgn30i9u0ploz0zrcqc3woojt7rhk0n2zovs2gi5yn8o4bom4kmcv_4j9k2z"))}
testObject_OtherMember_user_10 :: OtherMember
testObject_OtherMember_user_10 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001b-0000-0016-0000-000a00000017"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000200000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000003")))}), omConvRoleName = (fromJust (parseRoleName "zu9bajsdaigwabrzqfxkbxlxlo6jj7o3zaq_5hi2l6f4wlzlwn95fkhzk83t_nq90k86y43xw6jsh7zc1yfx_"))}
testObject_OtherMember_user_11 :: OtherMember
testObject_OtherMember_user_11 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000017-0000-0015-0000-001e00000014"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "p8vk1uvuztrhjrb843bf8n96mxhxd0t5u0bsaywawp2mn52ky3k3s_pgg3vyum1hdpdn3y44"))}
testObject_OtherMember_user_12 :: OtherMember
testObject_OtherMember_user_12 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000007-0000-000f-0000-001800000019"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000004")))}), omConvRoleName = (fromJust (parseRoleName "plhbawgbg9jez94_d0yloogdrl6poit_kg"))}
testObject_OtherMember_user_13 :: OtherMember
testObject_OtherMember_user_13 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001a-0000-0001-0000-000b0000000c"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000200000004"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000100000002")))}), omConvRoleName = (fromJust (parseRoleName "kvugb2tzcwel7cdw9droebrkqebrstnw8"))}
testObject_OtherMember_user_14 :: OtherMember
testObject_OtherMember_user_14 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000006-0000-0014-0000-00050000001d"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000000000002")))}), omConvRoleName = (fromJust (parseRoleName "5c6upur1_lkpbz6h8waasau7qizo1w0u2j0hm_r6105i0kpdsp"))}
testObject_OtherMember_user_15 :: OtherMember
testObject_OtherMember_user_15 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0020-0000-00120000001d"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000400000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000400000000")))}), omConvRoleName = (fromJust (parseRoleName "qj0d0be6i09ynqtlxnn45fl5cgtxitm_jjrcat_loyvkec96eeltsvb8usaczt5v0q642zzyepyhvwk58ux065mo0x0xljt0dplrxnf160vq9bl1e31j5mep"))}
testObject_OtherMember_user_16 :: OtherMember
testObject_OtherMember_user_16 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000000d-0000-0002-0000-000e00000008"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000002")))}), omConvRoleName = (fromJust (parseRoleName "1n0bz2eevcnh5jp2yum3kkc6wylzal3bz0nb1g7tsa43p_o1k9ta"))}
testObject_OtherMember_user_17 :: OtherMember
testObject_OtherMember_user_17 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000006-0000-0012-0000-000e0000001c"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000400000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000100000004")))}), omConvRoleName = (fromJust (parseRoleName "eh7anozt9tf9aozp1dx97tff8jmemjxfxjszaezavaaq2dqe6areshg3mqi_w4ub_f"))}
testObject_OtherMember_user_18 :: OtherMember
testObject_OtherMember_user_18 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001c-0000-0005-0000-00170000000a"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000200000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000003")))}), omConvRoleName = (fromJust (parseRoleName "lcvqc0m4sl6lyyv6pus5cdmu5_xn3x95_8eeeksnh30legt2v1eo9c569_9kw8my"))}
testObject_OtherMember_user_19 :: OtherMember
testObject_OtherMember_user_19 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0011-0000-000b00000019"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000000000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000004")))}), omConvRoleName = (fromJust (parseRoleName "a2v71hrhg"))}
testObject_OtherMember_user_20 :: OtherMember
testObject_OtherMember_user_20 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000005-0000-0000-0000-00100000001b"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000200000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000300000004")))}), omConvRoleName = (fromJust (parseRoleName "ocyyq0iq0gel6vme7chqwhkm2kox0gb2_nphdtamek25"))}
