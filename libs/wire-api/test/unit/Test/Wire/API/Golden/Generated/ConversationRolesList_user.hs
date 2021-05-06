{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConversationRolesList_user where

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
testObject_ConversationRolesList_user_1 :: ConversationRolesList
testObject_ConversationRolesList_user_1 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "z145cu9t1lwgharigr9scci8jeiz8398zvxiopqwcp0anmd")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wkd2zmrmecjcpu_t9_p7xvsfg79s2b904pfpr0d06fukelbqda680om2tfsfc1zn1tl1se0md2oa0nxp94s494n9bi2p96jqu9iraw76o23")) (Just ((Actions (Set.fromList [RemoveConversationMember,ModifyConversationMessageTimer]))))))]}
testObject_ConversationRolesList_user_2 :: ConversationRolesList
testObject_ConversationRolesList_user_2 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "kjo4y6xnfjhsns7ng1mmkqy42ec7gjmpe37c1x9t502xjz2efzaxgwem43cyxiizx4t04ki8_6bwchhbh5xri2yk_vt1_")) (Just ((Actions (Set.fromList [DeleteConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "jdjzfettjfnyhta89m1a3iatyr7wgc6bi0842hlqolls229g1jmk4eh_j8pexyu_7vz2wak2evo8jiyxq_ifloqpf_i3lyzpxlmbl93sy_4a1x6dzgi7nik36")) (Just ((Actions (Set.fromList [DeleteConversation]))))))]}
testObject_ConversationRolesList_user_3 :: ConversationRolesList
testObject_ConversationRolesList_user_3 = ConversationRolesList {convRolesList = []}
testObject_ConversationRolesList_user_4 :: ConversationRolesList
testObject_ConversationRolesList_user_4 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_5 :: ConversationRolesList
testObject_ConversationRolesList_user_5 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "m5phs8trfpuj7mt12or56dair_n914p9p8eb___tutrufaeix9vl0fvdionpql09i0nwpztocs4cs3fi1quu4ht4or_9b9rpxbx52c9")) (Just ((Actions (Set.fromList [ModifyConversationName,LeaveConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "uccyvik8tq2ynbh2y1hf_ec62cz41ekkmfeo1d3plcfuykzx_f1kh0yaxlrip11a0k0fkrqmavbtc")) (Just ((Actions (Set.fromList [ModifyConversationAccess,LeaveConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "2fga0_5kywe__0d1knvgxm3b_qr47achqo1tf6im95l_0n1z52rlar2_287c6pjfx2tjb4728p")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_6 :: ConversationRolesList
testObject_ConversationRolesList_user_6 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "4d7gw5k7xw0_a17")) (Just ((Actions (Set.fromList [ModifyConversationReceiptMode])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "ceznkczz48n0htxi0n17odvv8c5o0l7oioknfcd566ran45zyq97d5of0")) (Just ((Actions (Set.fromList []))))))]}
testObject_ConversationRolesList_user_7 :: ConversationRolesList
testObject_ConversationRolesList_user_7 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_8 :: ConversationRolesList
testObject_ConversationRolesList_user_8 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "ravr6b")) (Just ((Actions (Set.fromList [AddConversationMember,RemoveConversationMember,ModifyConversationAccess,DeleteConversation]))))))]}
testObject_ConversationRolesList_user_9 :: ConversationRolesList
testObject_ConversationRolesList_user_9 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "xazl4llwo38e8r7f3ly3orurwnx1f5_daqywqi0aauh0fsftg85plrnnm4w_ylbrw3c3olo")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "o_qdff2fne__b4bq43d8r4w2d5jcug070i9v4d8hz6c0f7")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "mvyatewk3fr1lx6xhhwp0")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "vpfijd_juiboh91pc3t8ejp8z850lmton7wgnbbdsoo_lw3whyseaw666hxkqix9u0_rxb_yzwff69yk55_ncp9")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "pa_mp7y5jyxko7uoe2o_48g2g06btidq4pe16v62nhq")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_10 :: ConversationRolesList
testObject_ConversationRolesList_user_10 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "96j3x5r756jaiqrsmz1hyxp7bmq6qjf4gwx0sqp6h6ciy5sj_f0trwfi9qz3xtjvluiek36cgk8wn3ec0qd9hfu_85ot805fr")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "dlalwlq_hh1_7o9_hfttka06xasj3q3ca1kk8j67uwe")) (Just ((Actions (Set.fromList [ModifyConversationReceiptMode]))))))]}
testObject_ConversationRolesList_user_11 :: ConversationRolesList
testObject_ConversationRolesList_user_11 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "2xq5cbu6_3kuji6436o7e0qcmf7_p6b9utpdhr7smhq9f0t_k7dcl996x1eqpe06rj8tkqaikkoy3fjad_a")) (Just ((Actions (Set.fromList [ModifyConversationMessageTimer])))))),(fromJust (toConvRole (fromJust (parseRoleName "i0r1qbpiqilnsg8l7qa76p9emrtefvnbfbp_9p8_aytz9tmkgvx6wououl6tpmu907b6bzezbizc47wa5tqccfxczhx5irg81ckkstwnm6sk_jikoy6mkl9wbjayyik")) (Just ((Actions (Set.fromList [RemoveConversationMember,ModifyConversationMessageTimer])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "4xpefjnnnvkrekh_ismwd9i978d46wbwqdevcc6csr24daupqm5nmoutu2a8s87epub9drnk44psffu711ypqh5hltngsealryfu0ofmsl6xqplr65dz")) (Just ((Actions (Set.fromList []))))))]}
testObject_ConversationRolesList_user_12 :: ConversationRolesList
testObject_ConversationRolesList_user_12 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "dak27zfvw9uw90sbc0un3op7hjll6tihjhyyn0_zkfue28t9tpe_uwlxv53x27o4fi9gg9wmamw9no6msr5had8z6ri1u2pls9d")) (Just ((Actions (Set.fromList [ModifyOtherConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "xgv")) (Just ((Actions (Set.fromList [ModifyConversationName,DeleteConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_13 :: ConversationRolesList
testObject_ConversationRolesList_user_13 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_14 :: ConversationRolesList
testObject_ConversationRolesList_user_14 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "h6902dez1lrtp3mp3sqthjze0_lsuv9jnfwb7lt2y3kszar8id5ig04t28g_imj46z8aiu1z")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "0n5alvxq8tzxgnokymqhun1vtx8sz6")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "rhodk2_068f1zrw2jjj189v42xt4gwrirmb5i61xm_70grsc723xnzizeukvov2k4d4b9w9dcb2j7incbsoedz")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "5sw")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "ia0syx93ytqeyq73qtpxnq5f9j_kl0pczesi_")) (Just ((Actions (Set.fromList []))))))]}
testObject_ConversationRolesList_user_15 :: ConversationRolesList
testObject_ConversationRolesList_user_15 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_16 :: ConversationRolesList
testObject_ConversationRolesList_user_16 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_17 :: ConversationRolesList
testObject_ConversationRolesList_user_17 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "5jqtxynz7pxl9a6jh_2zcfc_lwnphf4r55e4rkpitmq3axw7h66gfisvee_km0l1fjv8lmyquqboelluy")) (Just ((Actions (Set.fromList []))))))]}
testObject_ConversationRolesList_user_18 :: ConversationRolesList
testObject_ConversationRolesList_user_18 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "aric")) (Just ((Actions (Set.fromList [AddConversationMember,ModifyConversationReceiptMode,DeleteConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_19 :: ConversationRolesList
testObject_ConversationRolesList_user_19 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "n6o1qjvj2hr3m_t8chuvl9rt_1tr8kuyxm7xtb9vzod_osk337n6e20gf78ox1uc85wkm0t6yjks25vrbacqfv")) (Just ((Actions (Set.fromList [ModifyConversationReceiptMode,DeleteConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "vis_5mugqtngcdu7gjf4oc_vw7atxo8w9sep1scb5avp4rk01bmhok8mgfnv1_8lxro2y2y_zyro0ganbj4hn5mo7ifbon6b55on33qm4j7mdv7fd811qxa58zu39")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_20 :: ConversationRolesList
testObject_ConversationRolesList_user_20 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "nnqey5eks2iackxlm0fbps8tdz9k5j7re2")) (Just ((Actions (Set.fromList [RemoveConversationMember]))))))]}
