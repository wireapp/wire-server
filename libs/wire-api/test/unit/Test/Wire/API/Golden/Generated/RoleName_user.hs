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
testObject_RoleName_user_1 = (fromJust (parseRoleName "luscb4f5tth_kihngyr1k_"))
testObject_RoleName_user_2 :: RoleName
testObject_RoleName_user_2 = (fromJust (parseRoleName "50w60t3uxjr3thwtls9j_29hcp"))
testObject_RoleName_user_3 :: RoleName
testObject_RoleName_user_3 = (fromJust (parseRoleName "1crkdcd2vg6m8hagfqu4ldjhie8_s68cqak3u5xllhn02egi2ata4y4f7bfop"))
testObject_RoleName_user_4 :: RoleName
testObject_RoleName_user_4 = (fromJust (parseRoleName "vficihdjux6drguu62kd3h40cua6z66gxn7zy3w"))
testObject_RoleName_user_5 :: RoleName
testObject_RoleName_user_5 = (fromJust (parseRoleName "_9r74t5teocm0uy6b2sgbwba4jjya44rg44rnewinmy7t8hf"))
testObject_RoleName_user_6 :: RoleName
testObject_RoleName_user_6 = (fromJust (parseRoleName "pw6bue0g2uug3ngha1iaiv0jo0c1x7ok09it0y3r4yyassn19o3il0g9bw_mbtg3usg27z2btmq9x9_kg20u50ggmv"))
testObject_RoleName_user_7 :: RoleName
testObject_RoleName_user_7 = (fromJust (parseRoleName "wbd_3_bdghcibdc4g2yoj7grykiausu9iba9p7n8lfq520o0lc77iphmi2x5c81_6g11yq_8lmtazoyfuhj"))
testObject_RoleName_user_8 :: RoleName
testObject_RoleName_user_8 = (fromJust (parseRoleName "yc85uar0ol7yqh2c2yjncs0py4mv2a9h06dxkqhjt0ukx0x5ojxyifzjqi42p9yx_5ca3dfv68y4"))
testObject_RoleName_user_9 :: RoleName
testObject_RoleName_user_9 = (fromJust (parseRoleName "odzf4zvbulpy1y0h8bn3rqolaxnssev1b0x16fjhisutbun4ugfv1m7uft"))
testObject_RoleName_user_10 :: RoleName
testObject_RoleName_user_10 = (fromJust (parseRoleName "88_sr7qv91v5l3kqipc304eb1go588l3r1uzybwaxkezt762tud1sgqkzvl"))
testObject_RoleName_user_11 :: RoleName
testObject_RoleName_user_11 = (fromJust (parseRoleName "ux6cgcgtt986rkqqy76ajtaem0572viu3vyzc81dm_z01midmor3jy0ectqlq9nj2k_7fge8gbfs7uba43jb6yf7242ew7a8u7i8lyfkaht78exjz_53hluc12f83dmy"))
testObject_RoleName_user_12 :: RoleName
testObject_RoleName_user_12 = (fromJust (parseRoleName "tfjqqy1gl71g"))
testObject_RoleName_user_13 :: RoleName
testObject_RoleName_user_13 = (fromJust (parseRoleName "7xsdwa4owcr"))
testObject_RoleName_user_14 :: RoleName
testObject_RoleName_user_14 = (fromJust (parseRoleName "zha40awx"))
testObject_RoleName_user_15 :: RoleName
testObject_RoleName_user_15 = (fromJust (parseRoleName "i9qu34mfsy5kpx13v8l5y_7y6ft0h_7yklwxnxrwa1tgpbwqpxauyg3o2_ma0y1acnyohqvjg__86ve1ijccqe8tfn3redo8b0dt8sape__9dgd2biiq"))
testObject_RoleName_user_16 :: RoleName
testObject_RoleName_user_16 = (fromJust (parseRoleName "hxi409px9hkufdq_5_2wqkrvmu0vx7hz680xnekvl3oc60ifz5cokx"))
testObject_RoleName_user_17 :: RoleName
testObject_RoleName_user_17 = (fromJust (parseRoleName "141dorloz3_6asfqs9n2_muckmaxtlmou_prv5bc1wjg7dk9ei3mukoy_gfdz7cgs6m1ga779agmaz5nxvyfrdp0uv"))
testObject_RoleName_user_18 :: RoleName
testObject_RoleName_user_18 = (fromJust (parseRoleName "k970onx3a_lhuxyoin5bpdsndzjwzwdl2t3je0bx41frcq9jnv3be0s0u0_6jrkldzzpcbmupr_z0k15mlzujwkzf06e3sewe"))
testObject_RoleName_user_19 :: RoleName
testObject_RoleName_user_19 = (fromJust (parseRoleName "y547qvwsj8cgu9faq5ctr86w04ls4w3rhn2ht7a3gi2al7du7j22y29pljoocga2mv4bpt"))
testObject_RoleName_user_20 :: RoleName
testObject_RoleName_user_20 = (fromJust (parseRoleName "xk41w2czw_w_rwasp33mfpctqjp7r_7xnjovt9nkb01lmc8e4z6axt_nyzo"))
