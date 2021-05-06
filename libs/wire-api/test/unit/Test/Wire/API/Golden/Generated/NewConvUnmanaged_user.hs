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
testObject_NewConvUnmanaged_user_1 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "8", newConvAccess = Set.fromList [], newConvAccessRole = Just PrivateAccessRole, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 3428414700297278}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "gljr4ptb4lhpsk75zcu0hxi1mauuoz2vies7g1neqzhtt7tk8xn8qw8idlg658qiam6fuerdffdmxhtx664zhl"))})
testObject_NewConvUnmanaged_user_2 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_2 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000")))], newConvName = Just "'t\b", newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 7547795206647320}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 2}), newConvUsersRole = (fromJust (parseRoleName "qbrzonz1wtluy_1i4ffam1ubb4ag3b5b3d6w6dfsow3g2y45r5revxlqkkq38wkp6dgj38uvz0tn32trikfj0qoxyelos"))})
testObject_NewConvUnmanaged_user_3 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_3 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))], newConvName = Just "", newConvAccess = Set.fromList [InviteAccess,LinkAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "kgcafbvp4x4i1kw_r5fzdq9a5jh_1pd3at4k4flochrlehznw5hoa60qlxx7sp9d0p0ucq_8v2lwg724a_0yfszu1r3m_xeqounlkn0gsfs"))})
testObject_NewConvUnmanaged_user_4 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_4 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))], newConvName = Just "\" \1083587", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 7253481030135841}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "zk29hfgcxgr6o9bzolrgj0ibifm8wezw1tf40y89"))})
testObject_NewConvUnmanaged_user_5 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_5 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001")))], newConvName = Nothing, newConvAccess = Set.fromList [PrivateAccess,LinkAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 5585171821585134}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "gdqv83z_f9nn0u6vpyeqrr675lnm62_wghkzpdfeko"))})
testObject_NewConvUnmanaged_user_6 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_6 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))], newConvName = Just "", newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 5666342154812633}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "h4tfz_demkpc12bf7bzckh22lhsu50_uiwndl6g9wcqlev_s9t949alzaibmph8mmv17gjx3diqi"))})
testObject_NewConvUnmanaged_user_7 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_7 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [PrivateAccess,InviteAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 4298714263068732}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "ophv_pc_7amf92qf9r4ju2nj5mjzbe7njqa_ki2b7peu6igw1q7882bp6117g_v8bd_0jh21tow81dtzur2f"))})
testObject_NewConvUnmanaged_user_8 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_8 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 4914436553335222}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "kdmu_1sask_v1g7x63dcpbunrpp5yv8w6kynxl08jlczmaezp5ew71hh3jn9q25makwzex93v5mddk9yzgxb5lnja_poy9som7i1hd1dux436zd"))})
testObject_NewConvUnmanaged_user_9 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_9 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Nothing, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 6593125950817726}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "53_sx3zv9kf_lg6b6ro9kbh4d88s9b6lu59"))})
testObject_NewConvUnmanaged_user_10 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_10 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "\SYN,", newConvAccess = Set.fromList [LinkAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 7673186778534289}), newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "y5toqrvxjb28xj0_w3tlhf0zyxrpjmfs2w14p7ij7a3wdps62_kthd7psiyrkorvq4c4476n29vzr0pmhq0vnoxxeyq1c9b"))})
testObject_NewConvUnmanaged_user_11 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_11 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))], newConvName = Nothing, newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "5dx4m42buw4fhnth4oq20rppcof0voljk0ogkgsf_tgj5ku75ly3ky81dhtot4"))})
testObject_NewConvUnmanaged_user_12 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_12 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))], newConvName = Just "%t", newConvAccess = Set.fromList [LinkAccess,CodeAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 8392720773072445}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "cg4maxiere8sn6w73ktwzsv1ee7ylvqz05ie5zscdpj9l3z6iho5_3ase_abi9u3lr_rv8v515ud3fjpb80d9ubwk9vfx7mjhe3_aj27qzw4awi_5o80"))})
testObject_NewConvUnmanaged_user_13 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_13 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))], newConvName = Nothing, newConvAccess = Set.fromList [], newConvAccessRole = Just TeamAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}), newConvUsersRole = (fromJust (parseRoleName "0r_z71gyriywx71ndbz9m9oodx3x482fsulpbfuuxuu5kj9uek_w2y5gyg_eun4fjr00iizao72411wgm4z4d5veqgqt6d3zu9e4z2wv_2hxud5_anceacorb1"))})
testObject_NewConvUnmanaged_user_14 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_14 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Nothing, newConvAccess = Set.fromList [PrivateAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Nothing, newConvMessageTimer = Just (Ms {ms = 1712465616993495}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "bgg3gx0nq"))})
testObject_NewConvUnmanaged_user_15 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_15 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002")))], newConvName = Just "t\173143i", newConvAccess = Set.fromList [], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 555884777579339}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -2}), newConvUsersRole = (fromJust (parseRoleName "clt19zz_n_0yk4k2rfd8m5x8_px6"))})
testObject_NewConvUnmanaged_user_16 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_16 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "%MP", newConvAccess = Set.fromList [PrivateAccess,LinkAccess,CodeAccess], newConvAccessRole = Just ActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), cnvManaged = False}), newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}), newConvUsersRole = (fromJust (parseRoleName "vviqg5a37ruzg281phcoy9e4akoufiyia9ds39ww"))})
testObject_NewConvUnmanaged_user_17 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_17 = NewConvUnmanaged (NewConv {newConvUsers = [], newConvName = Just "A,\1099560O", newConvAccess = Set.fromList [], newConvAccessRole = Just TeamAccessRole, newConvTeam = Nothing, newConvMessageTimer = Nothing, newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 3}), newConvUsersRole = (fromJust (parseRoleName "v72kmb65yvkii4v8akh9"))})
testObject_NewConvUnmanaged_user_18 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_18 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002")))], newConvName = Nothing, newConvAccess = Set.fromList [CodeAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 4201438420662542}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 0}), newConvUsersRole = (fromJust (parseRoleName "4fiu4jyke3vwbl4_uxluixg_d4cz"))})
testObject_NewConvUnmanaged_user_19 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_19 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))], newConvName = Just "", newConvAccess = Set.fromList [InviteAccess,CodeAccess], newConvAccessRole = Just NonActivatedAccessRole, newConvTeam = Nothing, newConvMessageTimer = Nothing, newConvReceiptMode = Nothing, newConvUsersRole = (fromJust (parseRoleName "vebni6wc52cjpbib5nuhz_22o87gjrhswyzszbr1djcjs2h94yy9lho2ttpl8azmy1a6z3e_thful26gq5ucpv1w"))})
testObject_NewConvUnmanaged_user_20 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_20 = NewConvUnmanaged (NewConv {newConvUsers = [(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))], newConvName = Just "\1099274\170801\1049648L", newConvAccess = Set.fromList [PrivateAccess,InviteAccess,LinkAccess,CodeAccess], newConvAccessRole = Nothing, newConvTeam = Just (ConvTeamInfo {cnvTeamId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))), cnvManaged = False}), newConvMessageTimer = Just (Ms {ms = 6449312333766073}), newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 4}), newConvUsersRole = (fromJust (parseRoleName "1qplh3bh99ld5d6fofb4slp"))})
