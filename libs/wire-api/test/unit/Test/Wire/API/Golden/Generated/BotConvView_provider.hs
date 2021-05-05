{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.BotConvView_provider where

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
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
import Wire.API.Provider.Service.Tag
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_BotConvView_1 :: BotConvView
testObject_BotConvView_1 = (botConvView ((Id (fromJust (UUID.fromString "00000014-0000-0014-0000-001100000008")))) (Just "UpyS(\CAN") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "e5lj_4pjiw4pv"))}]))
testObject_BotConvView_2 :: BotConvView
testObject_BotConvView_2 = (botConvView ((Id (fromJust (UUID.fromString "00000018-0000-0015-0000-00050000000b")))) (Just "\nh\EM\EM") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "ll8iwxe7myputw_54kpjyl3zhu689zu34_fu434acvs7fq2axtb7lmbqmfvqhgfbuzikmkl"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "18cgp8wmumk0e60isa5101p5kamanma7so7ohdg865urbg4lniymci33yh97dw5d"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "cs1iqtx4697n_fwimfhjokud46yjimdbk1"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "x984j9kmhf_l9lyhiz5hkl8g5l7_8anpoa3z18y6abraycetypy13dispf36rxpew8738rnimy5p0fqabgq"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "1aa7e29i_5ivknh84gr3zfylgh7efgs1in1496299odc4az12hjx56cmfw3828d0n6n5j8287ia9926h_k124j4upyoasszxzdt_s3mlieblprv9"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "gm3ux9q1b8xk5gab01txdu7c81yralitrv721id71fuopy0v9h9ebzy9c0is5oo87v9kshsoett3va5lhfb93t44h66423ta6dn4rilsoomzunuy43qmre1g"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "gdverukx_ji211517uar526vm7ol1aouq3bbttqr2_2v10nulkdc"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "f8yc2arhhgkgj37ica63e04m2wcy21przwsn10y7d3p4un39w3uprei3yhu_jklalfwfq"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "ui0xvgk3ycv60198u44c6sep73muxmkl2mv13ob3s2j5dzbmn5j95e5pki5y5d_xuvwq27g4y9d3vs9ujtg_d5hze27nnbok6y8vj95y_v5t3wrakozpy9h337tjdh"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), omConvRoleName = (fromJust (parseRoleName "kzxb18gzl6wzo9chid6oyjn5dbqtehabeq23drw8o0rck2_q7xt4p0zkk3tolbsm9bp35"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "cyggfesi6c7bvwjh01wtkch3d40uwvoqnjkxr3eiyn4blm0j2c2g4ofvtzmhne"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "j5q959c55i54rdhrogumbixwudzeuz6600hu94luor3an7dbld5ap0dugs07im9547wkmaqpqc5h5y0xj_oxxe0ou1mh135os51"))}]))
testObject_BotConvView_3 :: BotConvView
testObject_BotConvView_3 = (botConvView ((Id (fromJust (UUID.fromString "00000015-0000-0008-0000-001300000000")))) (Just "\1011221\fuGm1") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "8ch1x152t7s6qdeyyk_34c2w_i1gmodvjb9gur9u8bqtne9xm092pg0mxpzj7dj3qstvlpp0hlgclrkod38ak5etogn4o8zsjta494fzxpfml1_gkp9aweldz6t"))}]))
testObject_BotConvView_4 :: BotConvView
testObject_BotConvView_4 = (botConvView ((Id (fromJust (UUID.fromString "0000000d-0000-0002-0000-00120000000e")))) (Just "\159699\1107288>\1091787,") ([OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), omConvRoleName = (fromJust (parseRoleName "wozheveuv205kgg0rkvnxzxgjuj9_"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "1f3wcgg86x85p_2lkrgas1vy4mn2qti363ykiow417sehiw_lsrxmwbfvz1hhtw1uuyezcdzrbbov8w6gzhq"))}]))
testObject_BotConvView_5 :: BotConvView
testObject_BotConvView_5 = (botConvView ((Id (fromJust (UUID.fromString "0000001d-0000-0006-0000-001b00000013")))) (Just "") ([]))
