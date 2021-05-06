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
testObject_RoleName_user_1 = (fromJust (parseRoleName "679rh0ewjjyz6jagj_"))
testObject_RoleName_user_2 :: RoleName
testObject_RoleName_user_2 = (fromJust (parseRoleName "bf1rdtutdumo16gbkadf_3dylyvgq2cz0pa5ni"))
testObject_RoleName_user_3 :: RoleName
testObject_RoleName_user_3 = (fromJust (parseRoleName "xyurjb47mnf9en_es9bl94v6v9vcx5oxp4ns39nl0fuswr"))
testObject_RoleName_user_4 :: RoleName
testObject_RoleName_user_4 = (fromJust (parseRoleName "1r_vie5b_a164wy41joq1lekyjybo4muu50orj_z8vs40avzav8ioeoud0qg2ep"))
testObject_RoleName_user_5 :: RoleName
testObject_RoleName_user_5 = (fromJust (parseRoleName "bs4kc0_h5xxhwa"))
testObject_RoleName_user_6 :: RoleName
testObject_RoleName_user_6 = (fromJust (parseRoleName "p2ugu4yxvrvx84sc230yenhiecybl86x06uus4f_rq49svar34q5873h8q2xyb1ub48jee3cd2vybn6bktn"))
testObject_RoleName_user_7 :: RoleName
testObject_RoleName_user_7 = (fromJust (parseRoleName "dm3u8cysh9mcwu"))
testObject_RoleName_user_8 :: RoleName
testObject_RoleName_user_8 = (fromJust (parseRoleName "gyn2o2f4uivaqyf0npm1lj13dytflnbyi_yu8l0du9_i2eh5k1xf8uc3of9h8ve8n7x80epslgp6j"))
testObject_RoleName_user_9 :: RoleName
testObject_RoleName_user_9 = (fromJust (parseRoleName "yxrhyllge9z71yer09a5lkr7j0rnvnpsty0pwgj5k7foxtetu8hp_j0ygfzgq4z2krg1gc0n4qk5sygs0sjzztbbwawoh736pd__ljesswnea"))
testObject_RoleName_user_10 :: RoleName
testObject_RoleName_user_10 = (fromJust (parseRoleName "pax3kx7ylt14uvzzgckpoklshw90q7ju2wciwcfdnlhiurin2f1lde_h9oy8ry2lrramwxj2g"))
testObject_RoleName_user_11 :: RoleName
testObject_RoleName_user_11 = (fromJust (parseRoleName "zo484lugnrjp7gvt3owr7risssp"))
testObject_RoleName_user_12 :: RoleName
testObject_RoleName_user_12 = (fromJust (parseRoleName "1wvgt82hzqynnr6oog0trzpbdz_28__who93_8q6y55lfnh18o9csxziuh1d8b1tox531mus"))
testObject_RoleName_user_13 :: RoleName
testObject_RoleName_user_13 = (fromJust (parseRoleName "1qiboyzdtx6itab1q3l53pu0mbu34zsg0x1b13ew151dbqu0_xh8kbaolv75oikzgq760ojvljlatjg8mvgzpeoneka7ct7r684kdc"))
testObject_RoleName_user_14 :: RoleName
testObject_RoleName_user_14 = (fromJust (parseRoleName "azz__frlyhp0scgox2p3ti38n6ng1i5wcw_vnakqu_ia2urrx1wjn8pwcf07rm_7hls43o6v3u725bm2ueqeooy1q"))
testObject_RoleName_user_15 :: RoleName
testObject_RoleName_user_15 = (fromJust (parseRoleName "1e1ebpl1ggmcp_59lep8gh6l6omndb75fesbyugtsd1mh3k_rqozb1i85c2e2xf3ew9sdalstkzjeuli4yi1"))
testObject_RoleName_user_16 :: RoleName
testObject_RoleName_user_16 = (fromJust (parseRoleName "zqq4r6799u_1cy54xe8dprgeqn2ko8pfo96j8as_ut"))
testObject_RoleName_user_17 :: RoleName
testObject_RoleName_user_17 = (fromJust (parseRoleName "ff2m31q7_qda64g7dib30nyn5a2wmpg12zk1vtbrcrp7c13a0lj787j"))
testObject_RoleName_user_18 :: RoleName
testObject_RoleName_user_18 = (fromJust (parseRoleName "ewrgm_awgp7qfd31mkbjzssam9uu2q9f84gqanumfne9ktj12xacv6gbs0gfu5"))
testObject_RoleName_user_19 :: RoleName
testObject_RoleName_user_19 = (fromJust (parseRoleName "mpe95bgb8zsgkje2plrma6v9l2zddgncbz3hpyrvuvgqhstls5g7ytvq11akah73dd57sxywiqrxvjevku34atecrym1b6nr2mn3cj17b"))
testObject_RoleName_user_20 :: RoleName
testObject_RoleName_user_20 = (fromJust (parseRoleName "ql37hxiik0r1naq97421crek29ty03qllajlh3co6xm25szjd6phcxag6w5z5miy37"))
