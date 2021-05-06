{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RoleName_user where

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
testObject_RoleName_user_1 :: RoleName
testObject_RoleName_user_1 = (fromJust (parseRoleName "osrfqbxpz54b2__85wzd5g79611xunm8kwczgd96roo9b1x8fc0a_r5cbb4ik6fyb2blnfc"))
testObject_RoleName_user_2 :: RoleName
testObject_RoleName_user_2 = (fromJust (parseRoleName "baif3escd199qy2oo0rh6zwibhval9fdvisj5xubbi5a6526uuq5iy5nsn1s6eh3mwihayxodsed9j69u4pa5v8fy2d5xk3be"))
testObject_RoleName_user_3 :: RoleName
testObject_RoleName_user_3 = (fromJust (parseRoleName "0clg5izgdjk71vn0rbcf3nv0o_pul5xg9jwb5lplfgmvy605b44q2h8wvmp2ejw9v"))
testObject_RoleName_user_4 :: RoleName
testObject_RoleName_user_4 = (fromJust (parseRoleName "rfw785ooe1fybq9qtaycdem1mknrqozzuejith"))
testObject_RoleName_user_5 :: RoleName
testObject_RoleName_user_5 = (fromJust (parseRoleName "mxmstrydqpdihog552lwqsl9mqrutj5rfwltpu8cid20hesdsjqfi6bd1yv_crpx73pvs85ei8ej9z3ts_e90t9ohhrntk8iwpobru63p4v"))
testObject_RoleName_user_6 :: RoleName
testObject_RoleName_user_6 = (fromJust (parseRoleName "k4sp3p6d_wdcenn8gcoxgwv9gn9v1"))
testObject_RoleName_user_7 :: RoleName
testObject_RoleName_user_7 = (fromJust (parseRoleName "f9x7uubrdd2n80arhwsam0q1sw_7a0i77zcfbxip5o3n5qyii400ymfcm501c7ki_c49zu7ap0rscoe4cbq3o61lwos1dkeu671pdadekni7s3l_7y1mr3zqfktt"))
testObject_RoleName_user_8 :: RoleName
testObject_RoleName_user_8 = (fromJust (parseRoleName "t6zju0f_ox8rrwuxma2gsc"))
testObject_RoleName_user_9 :: RoleName
testObject_RoleName_user_9 = (fromJust (parseRoleName "exyfitp09m77x04397apk6bs2yv45dnlbz07cxwnt9vd5rqri67it0c540xugbyex1yo1oolywxuwip2lasiyww2l"))
testObject_RoleName_user_10 :: RoleName
testObject_RoleName_user_10 = (fromJust (parseRoleName "rukg9_2a8qfvbvdzlrw5vgb6mkc3cfqyv_dmsxlbz"))
testObject_RoleName_user_11 :: RoleName
testObject_RoleName_user_11 = (fromJust (parseRoleName "resgkb_hjeqobt_pe2exj7w2jyt6qriynvp7coo66wfu_b9pcu8gan0uvhhoy23cun0ex7fiq"))
testObject_RoleName_user_12 :: RoleName
testObject_RoleName_user_12 = (fromJust (parseRoleName "zdhva4rkdy0fuyu_gw5mi22jaliple8_7h2a_148w77jco525t_cqm4e_xv3whl"))
testObject_RoleName_user_13 :: RoleName
testObject_RoleName_user_13 = (fromJust (parseRoleName "pq38l9ojahrs9orfob_ly5xfn77c2eczlpjq4trvybnh0odxq55y6wtkhqgnywbnrmhoujtgk6_a_1deonz_z4hs1jxy9a42nmiz593ml17nbfhj_08_hilpapys6e5c"))
testObject_RoleName_user_14 :: RoleName
testObject_RoleName_user_14 = (fromJust (parseRoleName "bt672sugoblpcc1mgw1wsf5490ax6pa3bo5wc1t5nf_i9bs1jv0gl2m5yo5rgpjnou76ecyahng0honxs58r3lbtl_c4gpoc2lbvao8p5l7awtqwdve1ec"))
testObject_RoleName_user_15 :: RoleName
testObject_RoleName_user_15 = (fromJust (parseRoleName "eg321wslkjiap7_mquy_obang5403iqi57dlf53xgsh1loh0ts_x"))
testObject_RoleName_user_16 :: RoleName
testObject_RoleName_user_16 = (fromJust (parseRoleName "wwu4l2keyc00dmf9m00jdcap0t5li982pehctpzzjnzsyc50y5a74ylhqvda6fq9kfah6192ngsci6dm7c5txs45nqi2ub6nwy55_11v7l8"))
testObject_RoleName_user_17 :: RoleName
testObject_RoleName_user_17 = (fromJust (parseRoleName "2y1j0eh7u8uslobr7w6u8adatibfm1o_q1sj4r39yeefzqhu1"))
testObject_RoleName_user_18 :: RoleName
testObject_RoleName_user_18 = (fromJust (parseRoleName "wjo3wz1fabe0_zec4mh6j9quda5uk4w_hf_f7vtf739vaj_5q0h6rc0ajqt9fv22chpmo7110zbda__mrmmxl4vn93tkmxvhzf89izka8xilmrp5vcs"))
testObject_RoleName_user_19 :: RoleName
testObject_RoleName_user_19 = (fromJust (parseRoleName "ixddb_fdqeaqgoa6ou61ul5m8rwl"))
testObject_RoleName_user_20 :: RoleName
testObject_RoleName_user_20 = (fromJust (parseRoleName "f7g67pv7m0a6fuzi52k97vugmzrhu"))
