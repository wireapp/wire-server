{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.OtherMemberUpdate_user where

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
testObject_OtherMemberUpdate_user_1 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_1 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "y8tzn85usw3"))}
testObject_OtherMemberUpdate_user_2 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_2 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "763hcoq51p10dktweej9mh_8kr02rg250xyvgds3eg_26ny4ayrunpjgbbz3xy7yvsy7t6pv24rl1qposbikjus2xnb3om3c3utom52icds0n9t"))}
testObject_OtherMemberUpdate_user_3 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_3 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "5v_te_lggjgj80ag54fs"))}
testObject_OtherMemberUpdate_user_4 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_4 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "_kpa94j_3kiysjs1rr1hk2kpqvue8mkjmnevqexrgiugcpgbs5lap4xabddx5dv5wcpeavnz3bbykaon9y8q02q3vwrdcquuk"))}
testObject_OtherMemberUpdate_user_5 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_5 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "8usjj"))}
testObject_OtherMemberUpdate_user_6 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_6 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "bocqr0asovgs_jwr3y2tua1evyokbat83y7m"))}
testObject_OtherMemberUpdate_user_7 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_7 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "mxwm70gn9ns9tdbirri8skatt6yqfz5h224gxzpole2b4to3hg0truq21mks7s5oeq6s_8d3yezjnavqvkjo0i37ta5oqkkiamv"))}
testObject_OtherMemberUpdate_user_8 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_8 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "owkcr_m_ip4dzvpzs0b4jinjwb8278pbf1tm"))}
testObject_OtherMemberUpdate_user_9 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_9 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "tyo4k4yrdgezlg32d5e9anijfa3v2qd07zueja75uq7k6oqlla980y5umzab4ohcvvfigbok1bgo6"))}
testObject_OtherMemberUpdate_user_10 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_10 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "slvet8itu_cw_on56tl7ljhuengb4kqa1313_s4mxni4k8znhw3dvp2b7aobcbnq7mic9488vb4vgjzn0d5d5mlypbqvlv3gani7joxcryb2cy32"))}
testObject_OtherMemberUpdate_user_11 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_11 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "99q4hbiy6h8b3xvmpueom1py6euh1punsy4bd2tmr7ier0cpbusj3fialbtujuy5kv8xwoabpihltydymcbs_6y94e3vcp425qgpk0_m9nh502_7x025i1puj5e197p"))}
testObject_OtherMemberUpdate_user_12 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_12 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "uxgwjvj0cblzhwcrlqf0xohs1dyj58bq6do55qbic3t9k4gw239x6xvsv5ilvy78q9tikp5k74g0g7s7k8qkd75"))}
testObject_OtherMemberUpdate_user_13 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_13 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "wcapfttkg6lt5wlemr0"))}
testObject_OtherMemberUpdate_user_14 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_14 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "tedghef5tw5ajfqyykw_c1u33l_ipnsxv_f44c39h13zyre9i57r7261acsgart82m_i1b2ktgzsk8moc3t8f77gjdvqihnrmrz0zzu07ulzcm0pxrt8fys9_1d"))}
testObject_OtherMemberUpdate_user_15 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_15 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "9_369pwvs3xx7peko0cdnn634gnregyutc"))}
testObject_OtherMemberUpdate_user_16 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_16 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "mvn5wx4pr_knv144p255ila46nn01mcsws7hmqhnf5g33zbqjsr6k3zpk7_nw542cgsx0dnja4zyjzrh9sbuyas9x02_4hpzcpr_oj7ny4s7po0lu_k"))}
testObject_OtherMemberUpdate_user_17 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_17 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "33h1pv2qzgrh8ndflst71i612ph_eu36x4a1hjkjrxosds3ba0av8lhp4pe6b5"))}
testObject_OtherMemberUpdate_user_18 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_18 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "ed19tetewl_qpfu5ty4c0c6eppgj0gcx1kk699gba5j_vn272"))}
testObject_OtherMemberUpdate_user_19 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_19 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "uu5e9uwow5ho_zkiphgp5jrqfpaiqdv1pjoj813lpbr32tuh9owq0d0tls6nmmyzx40rkgo7u_mrm"))}
testObject_OtherMemberUpdate_user_20 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_20 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "qhtg_71php7sysr0imkke98furdd90_b0dk4ytkbqy0mbwqakq6hzskwtc9a5y3q34tv4h1o8nkyfb42s3gtfpikyqaytawvm5_e_puxe"))}
