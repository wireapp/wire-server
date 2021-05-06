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
testObject_OtherMember_user_1 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000000c-0000-000d-0000-001900000014"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "sm62t_9xpbinxwi5g0gb4_5mhi0scsexmmodg6cijamj__vdomt19v35qopcudlxxpcodh3pn3844tkcw3jczeh8"))}
testObject_OtherMember_user_2 :: OtherMember
testObject_OtherMember_user_2 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000017-0000-0012-0000-000b0000001d"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "14b2rjabylywfsqr1_b1bwuevc6ieyfebv_eks_z99v4h9tgf4k5uq8s7hqxvk7870e_ji3uzirlb5tqbexqxb9e9x8xb5g8v2w_ihwnf4m3ibgt9pdparhm_uc330k"))}
testObject_OtherMember_user_3 :: OtherMember
testObject_OtherMember_user_3 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001a-0000-001a-0000-000e0000000c"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "_x9en8i9f1h7sd0r0944ntew3hfwhlkd2gdknd1eqcrrhcz_oxgzi6sx1g0djwvl1y5lz9zc25rvnww4afchw9tbmsho"))}
testObject_OtherMember_user_4 :: OtherMember
testObject_OtherMember_user_4 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000018-0000-0017-0000-000400000017"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000100000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000004")))}), omConvRoleName = (fromJust (parseRoleName "hx49cgxtqc8dizo_wdbkw1nxw2k_9erb97jkcf"))}
testObject_OtherMember_user_5 :: OtherMember
testObject_OtherMember_user_5 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000006-0000-0006-0000-001d0000001f"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000003")))}), omConvRoleName = (fromJust (parseRoleName "niztiz8nigusw3ta429bhqwmwokmtalnbxwc8cznc_pzzp63qvw9rnfz2jj7tj0o1o_ebuof8awaun43kkv91fonog5wvt38iw53c_pagtsv_ug1625eqika6"))}
testObject_OtherMember_user_6 :: OtherMember
testObject_OtherMember_user_6 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000000d-0000-000f-0000-00190000001b"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "c3db05pj_sqqpf87z483vr8oqic35z7iyp"))}
testObject_OtherMember_user_7 :: OtherMember
testObject_OtherMember_user_7 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001e-0000-0017-0000-001500000004"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000400000001")))}), omConvRoleName = (fromJust (parseRoleName "pziyosi7de2hvi2ly4uok_v53cbf9meu2_b33yd8nktrmow15ibmd"))}
testObject_OtherMember_user_8 :: OtherMember
testObject_OtherMember_user_8 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000000a-0000-001b-0000-000f00000007"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "2p4atta_qu16hyoi_s5szfblv4zg2ndogqilj1zd3"))}
testObject_OtherMember_user_9 :: OtherMember
testObject_OtherMember_user_9 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001b-0000-0017-0000-00080000001e"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000400000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000003")))}), omConvRoleName = (fromJust (parseRoleName "ax9ozasux5czwuw7zynnvpvv2iatq6"))}
testObject_OtherMember_user_10 :: OtherMember
testObject_OtherMember_user_10 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000000c-0000-0018-0000-001b00000007"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "aemc4wbgez2r6_esifahstjtjqvft3n01vf6nulcw9"))}
testObject_OtherMember_user_11 :: OtherMember
testObject_OtherMember_user_11 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000000e-0000-0013-0000-000000000019"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000300000003")))}), omConvRoleName = (fromJust (parseRoleName "g_di7z_d508i3tv8p9rqnufbhwpg92zvlhupf5q57vqvuz5dnrwoemv"))}
testObject_OtherMember_user_12 :: OtherMember
testObject_OtherMember_user_12 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000011-0000-0016-0000-000c00000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000300000001")))}), omConvRoleName = (fromJust (parseRoleName "_nnnxa7oj_u9opa870l5i2ws5ufab7tyfsv9xy"))}
testObject_OtherMember_user_13 :: OtherMember
testObject_OtherMember_user_13 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000007-0000-001d-0000-000c00000011"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "5fg4yqe7ddrnlertn46zt6bdvcfy288xy4tb002poy_v4s4nd2ahcifk_eqe4"))}
testObject_OtherMember_user_14 :: OtherMember
testObject_OtherMember_user_14 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000019-0000-0002-0000-001500000017"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000300000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000300000002")))}), omConvRoleName = (fromJust (parseRoleName "i7fe0rv8ibrkjg_fayn_w6hr3i90kiz2q_t0_ivak5c1gs2t38avghmnuig190fgjd"))}
testObject_OtherMember_user_15 :: OtherMember
testObject_OtherMember_user_15 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001c-0000-001d-0000-001a00000002"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000004"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000003")))}), omConvRoleName = (fromJust (parseRoleName "chcakxscffwb496i3pr26f6u9cx43lg66tj8e5657oixs4p1ynzponncdsvo24eizo_gvzqmwvt14eh7uwk9tvn0v0oye1tjgm39w26g97y4c5fnf5"))}
testObject_OtherMember_user_16 :: OtherMember
testObject_OtherMember_user_16 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-001d00000006"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000400000004")))}), omConvRoleName = (fromJust (parseRoleName "9jx32jsogz3dyg7fu7fbp9u3gf0jnwyqahqeak7k5v5zqiyizda3ty776g3_cn0c03va_txi0sfkyo2ycf"))}
testObject_OtherMember_user_17 :: OtherMember
testObject_OtherMember_user_17 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000020-0000-0016-0000-00100000001e"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000200000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001")))}), omConvRoleName = (fromJust (parseRoleName "fa4h2c940y8t9t1qrsk_ze97eeh2v5l7a39onij1nopl717x9pbnnmzw3_a317dmhxn0ddc"))}
testObject_OtherMember_user_18 :: OtherMember
testObject_OtherMember_user_18 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000008-0000-001b-0000-001100000011"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "2yz2tt5eihdbkdfa_9ap3tyyl5f3ehl6_no5t_3d940ibxbxd"))}
testObject_OtherMember_user_19 :: OtherMember
testObject_OtherMember_user_19 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001f-0000-0009-0000-00060000000b"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000400000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000000")))}), omConvRoleName = (fromJust (parseRoleName "396pgno93yd89mddgguyox"))}
testObject_OtherMember_user_20 :: OtherMember
testObject_OtherMember_user_20 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000010-0000-0006-0000-001700000017"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "duvl75if72"))}
