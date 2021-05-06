{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LoginId_user where

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
testObject_LoginId_user_1 :: LoginId
testObject_LoginId_user_1 = LoginByEmail (Email {emailLocal = "\1053320\&6\38407\FS5A\FSMO\163349\1081662\&3F0\\9K\NUL", emailDomain = "E+N<b4\GSP^4L\DC1\1056575TW\US?\1026907o"})
testObject_LoginId_user_2 :: LoginId
testObject_LoginId_user_2 = LoginByPhone (Phone {fromPhone = "+25506273"})
testObject_LoginId_user_3 :: LoginId
testObject_LoginId_user_3 = LoginByPhone (Phone {fromPhone = "+11804140"})
testObject_LoginId_user_4 :: LoginId
testObject_LoginId_user_4 = LoginByPhone (Phone {fromPhone = "+248595702006438"})
testObject_LoginId_user_5 :: LoginId
testObject_LoginId_user_5 = LoginByEmail (Email {emailLocal = "c\ACK\t\997737U&V9.<A+o\1018047", emailDomain = "\SUBwU\nLt\FS\SOHn\DC1b\42854o \SUB\39923+\au \154504\1002759\ETB\27542\CAN\149932!)\1106567"})
testObject_LoginId_user_6 :: LoginId
testObject_LoginId_user_6 = LoginByEmail (Email {emailLocal = ",wcIF\995617\a\EOT\146926\&2[\ACK", emailDomain = "\NUL#\"(\152630\DC1Jg\33190K6\SUB><\NUL\174259\110962\1006600[S_\UST"})
testObject_LoginId_user_7 :: LoginId
testObject_LoginId_user_7 = LoginByEmail (Email {emailLocal = "", emailDomain = "Ptx2\52064Y\1092027YDG\1005513\1063178\46223\58966qL\44247z\DC4NX?\1007581"})
testObject_LoginId_user_8 :: LoginId
testObject_LoginId_user_8 = LoginByHandle (Handle {fromHandle = "s8w-i.5iu__mvcj-fenq4u3ndovvdt1trhvc8zkbhhnv9aj-ngoomz3i0jqf0gm0cfd0nx941giqu.dht3l7rc_qjfehars946.e8e0iv3idy9odprwxd1iymcy6e2k8e1xwen1rs0peltf9phczw9o3zpsl-ntuv.yf_3bkm0z0h72c5qr-lnxhfyssa37ts7iii8.8kbszvt6t9mg3yvap.n6rldkj.jxjj0b-1c.jae96"})
testObject_LoginId_user_9 :: LoginId
testObject_LoginId_user_9 = LoginByHandle (Handle {fromHandle = "zluk0jgwwo3g8v5dsdavzuvhnkfxdiqxdz5so27kw77hn6ba7up6.7dwn2sf8e8.xdeaawwq7nyk4h3cu0.mc9yokm5nm.inqtut5dn8n_ao75vhorjb2dnwhtsxw16ajw"})
testObject_LoginId_user_10 :: LoginId
testObject_LoginId_user_10 = LoginByEmail (Email {emailLocal = "\1029283\&1yL", emailDomain = "90O\SUBC"})
testObject_LoginId_user_11 :: LoginId
testObject_LoginId_user_11 = LoginByHandle (Handle {fromHandle = "ozwzrznwgp.fx61olyf.bqpda.fgy_n10hhn0hqn_aze1409yhahlh_s0joow6xyxj"})
testObject_LoginId_user_12 :: LoginId
testObject_LoginId_user_12 = LoginByHandle (Handle {fromHandle = "jevcrlv66k2p7h.df9gmkax1nvpuvux.zllttgj5f_hcoer2qwear70gzujn5eulvfg_yiby8ujy4yeityzu6kv_9h3kler5o1fnjrph.i3jm1mgddhh5e1-04bn85mlezxhwfgujfj"})
testObject_LoginId_user_13 :: LoginId
testObject_LoginId_user_13 = LoginByHandle (Handle {fromHandle = "x1dt"})
testObject_LoginId_user_14 :: LoginId
testObject_LoginId_user_14 = LoginByPhone (Phone {fromPhone = "+568798078"})
testObject_LoginId_user_15 :: LoginId
testObject_LoginId_user_15 = LoginByPhone (Phone {fromPhone = "+45914277432981"})
testObject_LoginId_user_16 :: LoginId
testObject_LoginId_user_16 = LoginByEmail (Email {emailLocal = "\1049860\1015575\DC3I\SO\165562\47901S \US\DC3.1$O+", emailDomain = "G\19348\&95\153925\DC2\71210J\SIm7\64177A{<|\"\SOHK\1073602Gv3\SI("})
testObject_LoginId_user_17 :: LoginId
testObject_LoginId_user_17 = LoginByHandle (Handle {fromHandle = "z0ybihfc605yjrzqq8-z51fxzptiudyo2itzqya2wfgb2prf_-tzmhd2_49w6kzt08f2yope049gw-knw15vr0gy5y_bxjcu0ywkdnw22gsx7uf9z-v_j5zyqlz5qh6aisk8o_x2eo9_5o17ebgxk.26r_.butjts27lhb070fyzb3"})
testObject_LoginId_user_18 :: LoginId
testObject_LoginId_user_18 = LoginByHandle (Handle {fromHandle = "etm4x6_0o"})
testObject_LoginId_user_19 :: LoginId
testObject_LoginId_user_19 = LoginByEmail (Email {emailLocal = "\NUL1\SYNi\b", emailDomain = "\20604\\\8145\US\178940\154624n\1037964\174951P\152402U\1023010F"})
testObject_LoginId_user_20 :: LoginId
testObject_LoginId_user_20 = LoginByPhone (Phone {fromPhone = "+394102905299"})
