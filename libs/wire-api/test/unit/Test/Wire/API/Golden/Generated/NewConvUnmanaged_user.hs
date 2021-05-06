{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewConvUnmanaged_user where

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
testObject_NewConvUnmanaged_user_1 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_1 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))], newConvName = Just "\ETB", newConvAccess = Set.fromList [], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 2507683483785243}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "1r08jrzi_ctclh1jr10hccckm24letrc6x8tcd3jhrl83r74u66ezxc3ywf4bnyf1g2j9v6"))})
testObject_NewConvUnmanaged_user_2 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_2 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "\STX", newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 4749691349930540}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "i8sifi9u9pwsemcj5e"))})
testObject_NewConvUnmanaged_user_3 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_3 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))], newConvName = Nothing, newConvAccess = Set.fromList [InviteAccess,CodeAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 960530718258359}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "f9atihrdubnbqcj7pikeapcwvvbxrkczsuee6qfhgvgc8_jcsqrcxia7ipv518kc01"))})
testObject_NewConvUnmanaged_user_4 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_4 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "A", newConvAccess = Set.fromList [], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "roj0zmhskmrri3a45_xqkb4u_2vv3cuof81w31_467wkmx1kxwt61f1ckjx0_78ocwee1816mqdx1a6onik5q5rym_8fbsrupe03alhmaxnte3c1oaq"))})
testObject_NewConvUnmanaged_user_5 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_5 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))], newConvName = Just "\1084744Im", newConvAccess = Set.fromList [InviteAccess,LinkAccess,CodeAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 4597112852848425}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "al47qt8wjr58z_yg8ii5oitnemm1zig76677o_p96jx_bf4sufgmjowv6vuo4j00qyv"))})
testObject_NewConvUnmanaged_user_6 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_6 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))], newConvName = Nothing, newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 7870382423439717}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "1imtu7"))})
testObject_NewConvUnmanaged_user_7 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_7 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "", newConvAccess = Set.fromList [PrivateAccess,CodeAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 1220981316430588}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "hu342gv4ob5xdo6bvmxlvg0ja84xbb0es2m3brix6x"))})
testObject_NewConvUnmanaged_user_8 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_8 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))], newConvName = Just "", newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 8244576033141802}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "nuapxsi4ccow_2ilm5fqmqsicns4xvh3vf6swtiqzd8iyzbi5voyw_g_yposesfoaqawailoidb"))})
testObject_NewConvUnmanaged_user_9 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_9 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000")))], newConvName = Just "J\53149", newConvAccess = Set.fromList [], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 4532381553418339}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "__siyn3v7ggvbs5"))})
testObject_NewConvUnmanaged_user_10 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_10 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "M\1023702", newConvAccess = Set.fromList [InviteAccess], newConvAccessRole = Nothing, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 8594796386705933}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "_0ov8vrhuyf9v9"))})
testObject_NewConvUnmanaged_user_11 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_11 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))], newConvName = Just "W", newConvAccess = Set.fromList [PrivateAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "193ahfndohjzjcc9q5s5pmw3f61jwcp5laco6uktx32zvtutdtf7bbsrkqjurql_i69d3"))})
testObject_NewConvUnmanaged_user_12 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_12 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "", newConvAccess = Set.fromList [PrivateAccess,LinkAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Nothing, newConvMessageTimer = Nothing, newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "68gg7n3hsifm6yxswkyg3rft8w9at2kv2naagz4twbpabh0vkoobq8wpts5n9x3gn1ph3qhw"))})
testObject_NewConvUnmanaged_user_13 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_13 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002")))], newConvName = Nothing, newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 820114757334106}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "ua4a6kkr7w8kzz3knejcn8ljx3o7p8_w6im_ddmp07qt0di7yo7ertjeo7iyol7erogiiw46k0azzfhmn45vzmor_26lb6l68uxbouhk975n90glpnoi7p"))})
testObject_NewConvUnmanaged_user_14 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_14 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))], newConvName = Just "\49801", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "xuwvjexjamty6mzkxqmvy6365tn8wg74in28isjs3_iqugw78cqtbf0o7ya88m42pdd8w5cgc9l3_l7pls6j7qkh43g701s5spacr3m2x2_o"))})
testObject_NewConvUnmanaged_user_15 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_15 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "\SUB[\135361", newConvAccess = Set.fromList [], newConvAccessRole = Just TeamAccessRole, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 7766714029807087}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "ed7wopz_6i61l2og7qhkqhx6dcq9s2qho_4d_r1fad7zaruvzh75zo5m6b63oe_d8ubtrrimzt8ycgiv8fftnf"))})
testObject_NewConvUnmanaged_user_16 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_16 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002")))], newConvName = Nothing, newConvAccess = Set.fromList [], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 2388601483764061}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "bvpp2wvu2gr_90x9z3z3m8muqxj10m4bdc_jbq1lqh3ozfnoasl6vzw51gxaspglkribdcdfhiyw1q86c0_s3phmz272xag4l4wow6"))})
testObject_NewConvUnmanaged_user_17 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_17 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "\996685u", newConvAccess = Set.fromList [PrivateAccess,LinkAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 4819270303938467}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "2tf1hizjwvl9hlm3hlws8eyc7ds9s_2ew8ytecnb2o_bepd9my6guj_m_vjp4dbasyjich2pct_ztsh9blgu87ok6ud_5yp7pe_gi41sxvezaugfpcvrhzzfy"))})
testObject_NewConvUnmanaged_user_18 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_18 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))], newConvName = Just "", newConvAccess = Set.fromList [PrivateAccess,InviteAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 1322757443851335}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "jx7zp2obr8f6fbu_2pjbvpgfhy0xrfadpfd4mfv3rl7dios_tzjlru738udaw_tqfrpt529qvdodnw5zc693duqewq7qnzi680060rsc1uf3hihwzpjk"))})
testObject_NewConvUnmanaged_user_19 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_19 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "_", newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 4820214028083551}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "pnur3cwbeeld9ppondxh5eq1o"))})
testObject_NewConvUnmanaged_user_20 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_20 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))], newConvName = Just "\ACKW\995249", newConvAccess = Set.fromList [PrivateAccess,LinkAccess], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "g_btvg1aseuevvtis7v60h0mfcebz1x71u9a3t7dwg8z1rgkmd0dy0tzc1g3js416zuror5du1texmxqekq6yrud6590ykbi3peww564f9k9ruke"))})
