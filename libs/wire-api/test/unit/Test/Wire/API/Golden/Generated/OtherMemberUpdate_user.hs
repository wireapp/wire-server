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
testObject_OtherMemberUpdate_user_1 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "_vno6pau9nhonyy7i3e3i1vmkc5a4mjxmudzqxjlf605n09to1mefss9pee98_uefildzolwraoemkizeoy9ko37wn1cf2lj0wwck7"))}
testObject_OtherMemberUpdate_user_2 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_2 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "dpptzlolq7x_x16rnb3jgkqc1y2olos7od1wpabfkw7tj40va2kb1obwk1z4g5tb85fc8memz9mcl4xdyiyeu3nkx_2fh4rkknlatx27yz3rjd5iqgh"))}
testObject_OtherMemberUpdate_user_3 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_3 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "h2_er22rdho_ylmo__wlt9dnxd8_6z425c"))}
testObject_OtherMemberUpdate_user_4 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_4 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "vy6_ig_t7f3strheuww9lldmswxfqotccd5di"))}
testObject_OtherMemberUpdate_user_5 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_5 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "dcggc0ja60pr_z7b9h47xg8d_b60k480wf7kut89u5l7zvc66_nt19nxux6ke7pxdmoxe2kz2h3qvqjl0xqui7ixn57"))}
testObject_OtherMemberUpdate_user_6 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_6 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "g23god9ex0yt418w_xvzhsvxhibxz4t042"))}
testObject_OtherMemberUpdate_user_7 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_7 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "js9es_4tssebyopyfsz9hdxf3x5_dxn5urmd6effgo9hog_ej"))}
testObject_OtherMemberUpdate_user_8 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_8 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "8wlyj4eoavz7naybs900aue7qtkak4r3m5ogc6ionllxblx25xg20ly8va_ekf99hq3o8g36ew2vsaenla_1lm3fg6e2fk_rabzbpdw3kraqatpse"))}
testObject_OtherMemberUpdate_user_9 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_9 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "5jn08f95c9gl"))}
testObject_OtherMemberUpdate_user_10 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_10 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "u8r3uv0swpfcy8lvjsyj66q4o0ftbf5bynntgba3jhvj57"))}
testObject_OtherMemberUpdate_user_11 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_11 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "d9q1z90hiqyl6t8dxufusour5yq_5gwgl8ku28kf9jegsrqov12aqni1su1eufhs0phyxqsaql9zuvgnd2ew_pp5q"))}
testObject_OtherMemberUpdate_user_12 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_12 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "6ps0_fni3cng2ektj_mk2gv6f6438pbbt2843g4nsrcnw39aklci2glg776iqswfhl41uqc5l5gs4kvyqwc0k8rzy1v75jwk9ju6ayslvl"))}
testObject_OtherMemberUpdate_user_13 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_13 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "jnabbe3vkir3gyvq90pb9dfirv3x6m0sizxjs69uautbu3hf8davzm_lnl92p8ar0664"))}
testObject_OtherMemberUpdate_user_14 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_14 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "khqg63j3_f56_uy6_h10v7b15k9ytofr5dbr7e9fobpubf4av1frgaxc"))}
testObject_OtherMemberUpdate_user_15 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_15 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "fpxx5tq5et8w5uccde20zwou9xi7m43c"))}
testObject_OtherMemberUpdate_user_16 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_16 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "bw8o84fz3"))}
testObject_OtherMemberUpdate_user_17 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_17 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "zjacqtt225hrlhi"))}
testObject_OtherMemberUpdate_user_18 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_18 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "0znzkep24wagee8pnia6eteqfcnkgq999anu1i_ibldc4"))}
testObject_OtherMemberUpdate_user_19 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_19 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "2vwf35n2aihf5wmz13jhog6_4vueruomiuuiv6hx3r0zoi8kqe8i031nfa27ja2th9ydwxi5uxr_wjg34lv870bnt__trrvzdyj5zls1"))}
testObject_OtherMemberUpdate_user_20 :: OtherMemberUpdate
testObject_OtherMemberUpdate_user_20 = OtherMemberUpdate {omuConvRoleName = Just (fromJust (parseRoleName "a3sqwtm4mvq6p221imh5_1db6zartluoa31wqnghs965ne3kzx4km55zkfmeqo_q4no6iezq_qojo"))}
