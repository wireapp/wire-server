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
testObject_ConversationRolesList_user_1 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "ec48p009fbvcqyt019hwdq0uurt0h3aa9ta3i1jm0g9_fvd9l")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "qmwynruxatns_evjewl8uw0wopwadk7rdowk4zlcigc46yac4ovst9pfty6cooutv0p")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "95_ts896b4yhm6iv67gxq_rtqpypy4egzjsyjo4ol_05q_lvl72u5onlrodkk")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "l8l5buodcx7le9yctt2gp4rlcb3sijp1jgemrkccsn_dc9xiq9p43enltqlp8xedcv1lv0ufee6k4npr98kt1ncb5t07mz98zixlsburd9")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "imdgaivddhbv8ti2s556gcg60zw6k8btsxabw3rswkz14fa343msxgy_haqmynrbfyuuxo2_o")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "7ov793o8vkbndjjei1pr1rlwc__g7sf8q5b6l75xa1jsjycmsiq_gyosc08c9vgw4891cszhfxlzic1uv9lovdhzgdw23gaivtj_zd37lazz0biy5ea4tby38co25a")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "_c9xt678gq0j79u1s_39kwq83wm1jhe33b28xjr2cy1jhur38r6kwwy9e635nw_tk3vo9o8wmso5oo3jm")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "9yoyo1rye3t1pglju")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_2 :: ConversationRolesList
testObject_ConversationRolesList_user_2 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_3 :: ConversationRolesList
testObject_ConversationRolesList_user_3 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_4 :: ConversationRolesList
testObject_ConversationRolesList_user_4 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "shbeeah1p04dfhlvmmv_kw6htzjin93m6plgkb7_0_2muj6plemm7rc5ixs08vgs95")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_5 :: ConversationRolesList
testObject_ConversationRolesList_user_5 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_6 :: ConversationRolesList
testObject_ConversationRolesList_user_6 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_7 :: ConversationRolesList
testObject_ConversationRolesList_user_7 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "lumptonj_bo3g4tri9ksxq3c245ah46tz35rnjsz19hla4mlb_m7xyo3rkqqzyac9it2aur69o")) (Just ((Actions (Set.fromList []))))))]}
testObject_ConversationRolesList_user_8 :: ConversationRolesList
testObject_ConversationRolesList_user_8 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "x9c50jzhb")) (Just ((Actions (Set.fromList [ModifyConversationReceiptMode])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "ss0q603ryg7o")) (Just ((Actions (Set.fromList [ModifyOtherConversationMember]))))))]}
testObject_ConversationRolesList_user_9 :: ConversationRolesList
testObject_ConversationRolesList_user_9 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "_r1xpcz_8rz2jfo70gtc4")) (Just ((Actions (Set.fromList [LeaveConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "eyzri2z8blx6k5b7ih08r4l11o8fu711epuuqw4rjgn5kekhhwa")) (Just ((Actions (Set.fromList [ModifyConversationName,ModifyOtherConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "4788ti0np1cjwi9_l4osx1no_4hel1j567yyhm2zvcn1dmfx83xwgm7w7lyq7y2qc3263fvc3tipqelsscau4vqatklq7p8t9hui6ksu3b3rj6b0vb_s_t")) (Just ((Actions (Set.fromList [LeaveConversation])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_10 :: ConversationRolesList
testObject_ConversationRolesList_user_10 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "2iopnnxovvn8k3vpa674mbjcv0")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "hwm8a11vvnol15v6voalw0hq19mrvyoli57q3y9t1qdduuxqaby1b073qt1_lnb40046hlp_2_bpn4qw8d954mp035rnpcldh1bl16orhm9p7ei7aqcj099iyxdxwfly")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "27lsbxpnrjemmr9uwh280zt5u28o37spyv5wj031qm05voll_5521p337gdqi50")) (Just ((Actions (Set.fromList []))))))]}
testObject_ConversationRolesList_user_11 :: ConversationRolesList
testObject_ConversationRolesList_user_11 = ConversationRolesList {convRolesList = []}
testObject_ConversationRolesList_user_12 :: ConversationRolesList
testObject_ConversationRolesList_user_12 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "dr65")) (Just ((Actions (Set.fromList [ModifyConversationMessageTimer])))))),(fromJust (toConvRole (fromJust (parseRoleName "oo30rz7icckl790tzje_05r2dxqcai1j9k7gudtr_8k6p0exj3uok6_zi")) (Just ((Actions (Set.fromList [ModifyConversationAccess])))))),(fromJust (toConvRole (fromJust (parseRoleName "wo2_ppcoomkwoqyakar_un3ja4sc21912q8ouuao8uog")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "vrit75_n7s13awn7iznhm3xgr4xqtqlfscokxhhw181u4wl")) (Just ((Actions (Set.fromList [RemoveConversationMember]))))))]}
testObject_ConversationRolesList_user_13 :: ConversationRolesList
testObject_ConversationRolesList_user_13 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "abp1dpvtihne6hfgp4yabh_832jq1fozaz255mlm224dx1uvvbp6k796l8anamovpfyqx0p0wjv9hji_1p0wed8jluuue1b2ork4")) (Just ((Actions (Set.fromList []))))))]}
testObject_ConversationRolesList_user_14 :: ConversationRolesList
testObject_ConversationRolesList_user_14 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "7l65z4t360i54l_fyovbctg3v689we4qcoab")) (Just ((Actions (Set.fromList [ModifyConversationAccess])))))),(fromJust (toConvRole (fromJust (parseRoleName "crlwy0jprn_mguhqxe_a2bhnfc4tmdnfcr0wnklix5c8awmp2cp2614q7s4lflw3lwmpg07uhsweevleb1gkloy_nc_yc")) (Just ((Actions (Set.fromList [ModifyConversationName,ModifyOtherConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "iit2ixlc9sxl468ylawmpt9g_6")) (Just ((Actions (Set.fromList [ModifyOtherConversationMember])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing))]}
testObject_ConversationRolesList_user_15 :: ConversationRolesList
testObject_ConversationRolesList_user_15 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "064yxdkq_88hllb9oq1may0xjtmazxgxvwofko1sg0uu75tye2xy85bubctq878q59afct4ejb11k4o4lhylqs3s3nh03vpg06m")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "h2hq5hgv8cdsci6n82")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_16 :: ConversationRolesList
testObject_ConversationRolesList_user_16 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "l3otiq80t6ga7vrwkxhqewbth3tft_shjix6b7h3g2ose4ifa5s")) (Just ((Actions (Set.fromList [])))))),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_admin")) Nothing))]}
testObject_ConversationRolesList_user_17 :: ConversationRolesList
testObject_ConversationRolesList_user_17 = ConversationRolesList {convRolesList = [(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "wire_member")) Nothing)),(fromJust (toConvRole (fromJust (parseRoleName "gl96mqe10tr8s5irieqzel6j71jw78oz9at5kewvh6ex9ra4m97bwkr5hgeej0t")) (Just ((Actions (Set.fromList [ModifyOtherConversationMember]))))))]}
testObject_ConversationRolesList_user_18 :: ConversationRolesList
testObject_ConversationRolesList_user_18 = ConversationRolesList {convRolesList = []}
testObject_ConversationRolesList_user_19 :: ConversationRolesList
testObject_ConversationRolesList_user_19 = ConversationRolesList {convRolesList = []}
testObject_ConversationRolesList_user_20 :: ConversationRolesList
testObject_ConversationRolesList_user_20 = ConversationRolesList {convRolesList = []}
